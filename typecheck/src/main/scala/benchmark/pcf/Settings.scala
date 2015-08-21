package benchmark.pcf

import java.util.Properties

/**
 * Created by oliver on 09.06.15.
 */
object Settings {
  private lazy val prop = {
    val p = new Properties()
    val loader = Thread.currentThread().getContextClassLoader
    val stream = loader.getResourceAsStream("benchmark.properties")
    p.load(stream)
    p
  }

  def apply(key: String): String = prop.getProperty(key)
}
