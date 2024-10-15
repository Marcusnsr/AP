-- Messages sent to workers.
data WorkerMsg
  = MsgWorkerJob JobId Job
  | MsgWorkerTerminate
