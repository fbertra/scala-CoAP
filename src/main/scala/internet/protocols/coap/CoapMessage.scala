package internet.protocols.coap

/*
 * Constants used internally
 * 
 * not visible to client API
 */
object CoapMessageConstants {
 // class code
  val RequestClassCode = 0
  val SuccessResponseClassCode = 2
  val ClientErrorResponseClassCode = 4
  val ServerErrorResponseClassCode = 5
  
  // type: Confirmable, Non-confirmable, Acknowledgement, 
  val ConfirmableType = 0
  val NonConfirmableType = 1
  val AcknowledgementType = 2
  val ResetType = 3
}


case class CoapOption(number: Int, value: Array[Byte])

/*
 *
 */
case class CoapMessage(
  msgType: Int, 
  code: Int,
  messageId: Int,
  token: Array[Byte],
  options: Array[CoapOption],
  payload: Array[Byte]
) {

  validate()

  def version = 1

  def tokenLength = token.size

  /*
   *
   */
  def codeC = code >> 5

  /*
   *
   */
  def codeDD = code & 0x1F
  
  /*
   *
   */
  def isEmpty: Boolean = code == 0

  /*
   *
   */
  def isRequest: Boolean = {
    codeC == 0 && codeDD > 0 
  }

  /*
   *
   */
  def isResponse: Boolean = {
    val codeClass = codeC

    codeClass >= 2 && codeClass <= 5  
  }

  /*
   *
   */
  def isReserved: Boolean = ! (isEmpty || isRequest || isResponse)

  /*
   *
   */
  def validate() {
    import CoapMessageConstants._

    if (isReserved)
      throw new CoapMessageFormatException ("Method Code is reserved: " + codeC + "." + codeDD)
    
    // section 4.1: empty message should contain no token, no options, no payload 
    if (
      isEmpty
      &&
      (
        token.size != 0 ||
        options.size != 0 ||
        payload.size != 0
      )
    )
      throw new CoapMessageFormatException ("Non Empty message")

    // table 1 section 4.2 and 4.3
    if (isRequest && ! (msgType == ConfirmableType || msgType == NonConfirmableType))
      throw new CoapMessageFormatException ("Request cannot be Acknowledgement and Reset")

    if (isResponse && msgType == ResetType)
      throw new CoapMessageFormatException ("Response cannot be Reset")

    if (isEmpty && msgType == NonConfirmableType)
      throw new CoapMessageFormatException ("Empty cannot be non confirmable")
  }
}
