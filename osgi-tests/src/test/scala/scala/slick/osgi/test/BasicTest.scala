package scala.slick.osgi.test

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Assert._
import org.ops4j.pax.exam
import org.ops4j.pax.exam.junit.{Configuration, ExamReactorStrategy, JUnit4TestRunner}
import org.ops4j.pax.exam.spi.reactors.AllConfinedStagedReactorFactory
import scala.slick.SlickException
import scala.slick.osgi.testutil._

@RunWith(classOf[JUnit4TestRunner])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
class BasicTest extends SlickOsgiHelper {

  @Configuration
  def config(): Array[exam.Option] = {
    standardOptions
  }

  /*
  @Test
  def everythingLoads(): Unit = println("Running PAX-Exam test.")


  @Test(expected=classOf[SlickException])  
  def canDoSomethingSlick(): Unit = {
    throw new SlickException("Test failure")
  }
  */

  @Test
  def testLiftedEmbedding: Unit = {
    import scala.slick.driver.H2Driver.simple._
    Database.forURL("jdbc:h2:mem:test-osgi") withSession { implicit session =>
      assertEquals("TEST-OSGI", Functions.database.run)
    }
  }
}