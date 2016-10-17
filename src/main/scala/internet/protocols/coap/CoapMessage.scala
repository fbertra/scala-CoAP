package internet.protocols.coap

object CoapMessage {
 // code: Method code, Response code
  val RequestClassCode = 0
  val SuccessResponseClassCode = 2
  val ClientErrorResponseClassCode = 4
  val ServerErrorResponseClassCode = 5
  
// type: Confirmable, Non-confirmable, Acknowledgement, 
  val ConfirmableType = 0
  val NNonConfirmableType = 1
  val AcknowledgementType = 2
  val ResetType = 3
}


case class CoapOption(number: Int, value: Array[Byte])
/*
 *
 */
case class CoapMessage(
  version: Int, 
  msgType: Int, 
  tokenLength: Int, 
  code: Int,
  messageId: Int,
  token: Array[Byte],
  options: Array[CoapOption],
  payload: Array[Byte]
)
