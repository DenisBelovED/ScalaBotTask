import java.util.UUID.randomUUID

object UUIDGenerator {
  def GetUUID(): Long = randomUUID().getMostSignificantBits & Long.MaxValue
}
