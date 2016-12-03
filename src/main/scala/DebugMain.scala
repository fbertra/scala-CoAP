import internet.protocols.coap.CoapEndpoint
import java.net.InetSocketAddress


object DebugMain {
  def main (args: Array [String]) = {
    val ep = new CoapEndpoint (10010)
    
    val to = new InetSocketAddress ("127.0.0.1", 9999)
    
    val payload = "xyz".getBytes

    ep.fireAndForget(payload, to)
  }
}