package services

import v1.post.MasterMonitorInfo

class MonitorCredentialService {
  def getInfo(host:String,jobName:String):Option[MasterMonitorInfo] = {
    if(host != "hostileHost") Some(MasterMonitorInfo(10, host, jobName))
    else None
  }
}
