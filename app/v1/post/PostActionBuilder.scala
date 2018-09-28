package v1.post


import javax.inject.Inject
import net.logstash.logback.marker.LogstashMarker
import play.api.{Logger, MarkerContext}
import play.api.http.{FileMimeTypes, HttpVerbs}
import play.api.i18n.{Langs, MessagesApi}
import play.api.libs.json.{Json, OFormat}
import play.api.mvc._
import play.api.mvc.Results.Unauthorized
import services.MonitorCredentialService
import utility.JwtUtility

import scala.concurrent.{ExecutionContext, Future}

/**
  * A wrapped request for post resources.
  *
  * This is commonly used to hold request-specific information like
  * security credentials, and useful shortcut methods.
  */
trait PostRequestHeader extends MessagesRequestHeader with PreferredMessagesProvider
class PostRequest[A](monitorInfo: MasterMonitorInfo,request: Request[A], val messagesApi: MessagesApi) extends WrappedRequest(request) with PostRequestHeader



case class MasterMonitorInfo(id: Int,
                             host: String,
                             JobName: String)

case class MasterMonitor(host: String, jobName: String)





/**
 * Provides an implicit marker that will show the request in all logger statements.
 */
trait RequestMarkerContext {
  import net.logstash.logback.marker.Markers

  private def marker(tuple: (String, Any)) = Markers.append(tuple._1, tuple._2)

  private implicit class RichLogstashMarker(marker1: LogstashMarker) {
    def &&(marker2: LogstashMarker): LogstashMarker = marker1.and(marker2)
  }

  implicit def requestHeaderToMarkerContext(implicit request: RequestHeader): MarkerContext = {
    MarkerContext {
      marker("id" -> request.id) && marker("host" -> request.host) && marker("remoteAddress" -> request.remoteAddress)
    }
  }

}

/**
  * The action builder for the Post resource.
  *
  * This is the place to put logging, metrics, to augment
  * the request with contextual data, and manipulate the
  * result.
  */
class PostActionBuilder @Inject()(messagesApi: MessagesApi, playBodyParsers: PlayBodyParsers)
                                 (implicit val executionContext: ExecutionContext)
    extends ActionBuilder[PostRequest, AnyContent]
    with RequestMarkerContext
    with HttpVerbs {

  implicit val formatMasterMonitor: OFormat[MasterMonitor] = Json.format[MasterMonitor]
  private val service = new MonitorCredentialService()

  override val parser: BodyParser[AnyContent] = playBodyParsers.anyContent

  type PostRequestBlock[A] = PostRequest[A] => Future[Result]

  private val logger = Logger(this.getClass)

  override def invokeBlock[A](request: Request[A],  block: PostRequestBlock[A]): Future[Result] = {
    // Convert to marker context and use request in block
    implicit val markerContext: MarkerContext = requestHeaderToMarkerContext(request)
    logger.trace(s"invokeBlock: ")

    val jwtToken = request.headers.get("jw_token").getOrElse("")
    if (JwtUtility.isValidToken(jwtToken)) {
      val payload = JwtUtility.decodePayload(jwtToken)
      if (payload.isDefined) {
        val masterMonitor = Json.parse(payload.get).validate[MasterMonitor].get
        val maybeMonitorInfo = service.getInfo(masterMonitor.host, masterMonitor.jobName)
        if (maybeMonitorInfo.isDefined) {
          block(new PostRequest(maybeMonitorInfo.get, request, messagesApi))
        } else {
          Future.successful(Unauthorized("Invalid credential"))
        }
      } else {
        Future.successful(Unauthorized("Invalid credential"))
      }
    } else {
      Future.successful(Unauthorized("Invalid credential"))
    }
  }
}

/**
 * Packages up the component dependencies for the post controller.
 *
 * This is a good way to minimize the surface area exposed to the controller, so the
 * controller only has to have one thing injected.
 */
case class PostControllerComponents @Inject()(postActionBuilder: PostActionBuilder,
                                               postResourceHandler: PostResourceHandler,
                                               actionBuilder: DefaultActionBuilder,
                                               parsers: PlayBodyParsers,
                                               messagesApi: MessagesApi,
                                               langs: Langs,
                                               fileMimeTypes: FileMimeTypes,
                                               executionContext: scala.concurrent.ExecutionContext)
  extends ControllerComponents

/**
 * Exposes actions and handler to the PostController by wiring the injected state into the base class.
 */
class PostBaseController @Inject()(pcc: PostControllerComponents) extends BaseController with RequestMarkerContext {
  override protected def controllerComponents: ControllerComponents = pcc

  def PostAction: PostActionBuilder = pcc.postActionBuilder

  def postResourceHandler: PostResourceHandler = pcc.postResourceHandler
}