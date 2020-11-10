package services

import java.io.InputStream

import drt.shared.CrunchApi.MillisSinceEpoch
import javax.script.{ScriptEngine, ScriptEngineManager}
import org.renjin.sexp.{DoubleVector, IntVector}
import org.slf4j.{Logger, LoggerFactory}
import org.specs2.mutable.Specification

import scala.util.Failure

class OptimiserSpec extends Specification {
  val manager: ScriptEngineManager = new ScriptEngineManager()
  val engine: ScriptEngine = manager.getEngineByName("Renjin")

  def loadOptimiserScript: AnyRef = {
    if (engine == null) throw new scala.RuntimeException("Couldn't load Renjin script engine on the classpath")
    val asStream: InputStream = getClass.getResourceAsStream("/optimisation-v6.R")

    val optimiserScript = scala.io.Source.fromInputStream(asStream)
    engine.eval(optimiserScript.bufferedReader())
  }

  "leftward.desks comparison" >> {
    loadOptimiserScript

    val workloads = (1 to 360).map(_ => (Math.random() * 20))
    val minDesks = List.fill(360)(1)
    val maxDesks = List.fill(360)(10)
    engine.put("workload", workloads.toArray)
    engine.put("minDesks", minDesks.toArray)
    engine.put("maxDesks", maxDesks.toArray)

    engine.eval("results <- leftward.desks(workload, xmin=minDesks, xmax=maxDesks, block.size=5, backlog=0)$desks")
    val rResult = engine.eval("results").asInstanceOf[DoubleVector].toDoubleArray.map(_.toInt).toList

    val newResult = Optimiser.leftwardDesks(workloads.toIndexedSeq, minDesks.toIndexedSeq, maxDesks.toIndexedSeq, 5, 0)

    rResult === newResult
  }

  "process.work comparison" >> {
    loadOptimiserScript

    val desks = List(9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 4, 4, 4, 4, 4, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 8, 8, 8, 8, 8, 5, 5, 5, 5, 5, 15, 15, 15, 15, 15, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    val work: IndexedSeq[Double] = IndexedSeq[Double](10, 10, 17, 3, 9, 9, 14, 13, 5, 7, 18, 4, 16, 8, 6, 1, 3, 14, 9, 9, 8, 9, 1, 18, 11, 15, 13, 6, 12, 10, 19, 16, 2, 9, 10, 19, 14, 3, 5, 14, 4, 10, 12, 12, 4, 2, 3, 5, 12, 17, 16, 17, 14, 6, 4, 2, 8, 11, 13, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    engine.put("desks", desks.toArray)
    engine.put("work", work.toArray)

    engine.eval("results <- process.work(work, desks, 18, 0)$wait")
    val rResult = engine.eval("results").asInstanceOf[IntVector].toIntArray.toList

    val newResult = Optimiser.tryProcessWork(work, desks.toIndexedSeq, 18, IndexedSeq(0)).map(_.waits).get

    rResult === newResult
  }

  0 to 10 map { i =>
    s"leftward.desks comparison with random workload #$i" >> {
      loadOptimiserScript

      val winWork = (1 to 360).map(_ => (Math.random() * 20))
      val winXmin = IndexedSeq.fill(winWork.size)(1)
      val winXmax = IndexedSeq.fill(winWork.size)(9)
      val blockSize = 5

      val rDesks = OptimiserRInterface.leftwardDesksR(winWork, winXmin, winXmax, blockSize, backlog = 0d)
      val sDesks = Optimiser.leftwardDesks(winWork, winXmin, winXmax, blockSize, backlog = 0d)

      rDesks === sDesks
    }
  }

  "block.mean comparison" >> {
    skipped("exploratory")

    loadOptimiserScript

    val stuff = 1 to 100
    val blockWidth = 25
    engine.put("stuff", stuff.toArray)
    engine.put("blockWidth", blockWidth)
    engine.eval("block.mean(stuff, blockWidth)").asInstanceOf[DoubleVector].toDoubleArray

    success
  }

  "test stuff" >> {
    skipped("exploratory")

    loadOptimiserScript

    val workloads = (1 to 10)
    val cap = (1 to 10).map(_ => 2)
    engine.put("work", workloads.toArray)
    engine.put("cap", cap.toArray)
    engine.eval("((1 - work) * cap)").asInstanceOf[DoubleVector].toDoubleArray.mkString(", ")

    success
  }

  "optimise.win comparison" >> {
    skipped("exploratory")

    loadOptimiserScript

    val workloads = (1 to 720).map(_ => (Math.random() * 20))
    val maxDesks = (1 to 720).map(_ => 10)
    val minDesks = (1 to 720).map(_ => 1)
    val adjustedSla = (0.75d * 25).toInt

    val weightChurn = 50
    val weightPax = 0.05
    val weightStaff = 3
    val weightSla = 10

    engine.put("work", workloads.toArray)
    engine.put("xmax", maxDesks.toArray)
    engine.put("xmin", minDesks.toArray)
    engine.put("sla", adjustedSla)

    engine.put("w_churn", weightChurn)
    engine.put("w_pax", weightPax)
    engine.put("w_staff", weightStaff)
    engine.put("w_sla", weightSla)

    engine.eval("result <- optimise.win(work=work, xmin=xmin, xmax=xmax, sla=sla, weight.churn=w_churn, weight.pax=w_pax, weight.staff=w_staff, weight.sla=w_sla)")
    val rResult = engine.eval("result").asInstanceOf[DoubleVector].toDoubleArray.map(_.toInt).toSeq

    val newResult = Optimiser.tryOptimiseWin(workloads, minDesks.toIndexedSeq, maxDesks.toIndexedSeq, adjustedSla, weightChurn, weightPax, weightStaff, weightSla)

    newResult === rResult
  }

  "Given 10 minutes of work and 15 minutes of capacity" >> {
    "When I ask for the work to be optimised" >> {
      "I should get a failed result" >> {
        val work = List.fill(10)(1d)
        val capacity = List.fill(15)(10)
        val result = Optimiser.crunch(work, capacity, capacity, OptimizerConfig(25))

        result must haveClass[Failure[Exception]]
      }
    }
  }

  "Given 10 minutes of work and 15 minutes of capacity" >> {
    "When I ask for the work to be processed" >> {
      "I should get a failed result" >> {
        val work = List.fill(10)(1d)
        val desks = List.fill(15)(10)
        val result = Optimiser.runSimulationOfWork(work, desks, OptimizerConfig(25))

        result must haveClass[Failure[Exception]]
      }
    }
  }

  def lhrSlowWorkload2 = List(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 6.240209790209743, 6.240209790209743, 6.240209790209743, 6.240209790209743, 6.240209790209743, 19.98440656192364, 19.98440656192364, 19.98440656192364, 19.672396072413154, 13.7441967717139, 29.9681285468245, 29.968128546824495, 29.968128546824495, 29.968128546824495, 29.96812854682449, 29.968128546824495, 29.968128546824495, 18.972771129453378, 16.223931775110593, 16.223931775110593, 16.223931775110593, 12.017331775110595, 6.875931775110594, 6.875931775110594, 6.875931775110594, 3.094169298799767, 0.0, 0.0, 0.0, 0.0, 1.7688341669872711, 1.7688341669872711, 1.7688341669872711, 1.7688341669872711, 1.7688341669872711, 15.798799323781749, 15.798799323781747, 15.798799323781749, 15.798799323781749, 15.798799323781749, 5.421775343457819, 5.421775343457819, 5.421775343457819, 5.421775343457819, 5.421775343457819, 5.421775343457819, 5.421775343457819, 5.0680085100603645, 3.6529411764705473, 3.6529411764705473, 9.48053942974567, 9.48053942974567, 9.48053942974567, 9.48053942974567, 9.48053942974567, 9.48053942974567, 7.732259953763133, 0.3652941176470547, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.127626850761645, 4.127626850761645, 4.127626850761645, 4.1276268507616445, 4.1276268507616445, 6.829548039820339, 6.829548039820339, 6.829548039820338, 6.186791579366146, 5.972539425881415, 15.634385763911068, 14.604307186225444, 11.347397288020986, 10.599102827090297, 10.238456507521176, 19.586456507521177, 19.586456507521177, 15.847256507521177, 8.190765206016941, 0.0, 1.2000000000000015, 1.2000000000000015, 0.36000000000000043, 0.0, 0.0, 1.9955105733348408, 1.9955105733348408, 1.9955105733348408, 1.9431674771005314, 0.9486486486486503, 23.62064072449416, 7.657847460025468, 4.785081081081072, 4.785081081081072, 1.914032432432429, 12.869843562118863, 12.869843562118863, 12.869843562118863, 12.869843562118863, 11.588410666054285, 12.571298005492684, 12.571298005492686, 12.571298005492686, 12.208591238575394, 11.362275449101713, 6.9932432432432465, 6.9932432432432465, 0.6993243243243248, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0894117647058745, 1.0894117647058745, 1.0894117647058745, 1.0894117647058745, 1.0894117647058745, 11.526784027479628, 11.526784027479628, 11.526784027479628, 10.764195792185518, 10.437372262773755, 16.422759082257976, 16.422759082257976, 6.50725543262291, 5.985386819484222, 5.985386819484222, 10.477374920679473, 5.389796124117885, 5.389796124117885, 5.389796124117885, 5.389796124117885, 5.389796124117885, 5.351743026772753, 4.628734177215233, 4.628734177215233, 4.628734177215233, 6.530518156285872, 5.318999531644037, 5.1843863511282775, 4.96212548156306, 4.628734177215233, 3.9256776636858324, 0.6362608695652183, 0.0, 0.0, 0.0, 3.144503352565105, 2.8097926087634524, 1.8056603773584932, 0.3611320754716986, 0.0, 1.181739130434783, 0.7090434782608697, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.2108317492199, 1.681750116566838, 1.1720562390158173, 1.1720562390158173, 1.1720562390158173, 2.5720562390158195, 0.7472056239015827, 0.0, 0.0, 0.0, 1.5484931506849333, 0.15484931506849334, 0.0, 0.0, 0.0, 2.3202416918429027, 2.3202416918429027, 2.3202416918429027, 1.5081570996978868, 0.0, 1.0700917431192667, 1.0700917431192667, 1.0700917431192667, 0.8025688073394502, 0.0, 1.2197183098591564, 1.2197183098591564, 1.2197183098591564, 1.2197183098591564, 0.9757746478873253, 0.3127272727272731, 0.0, 0.0, 0.0, 0.0, 1.557561633754453, 1.557561633754453, 1.557561633754453, 0.9794971176254195, 0.8349809885931612, 1.2045908448559957, 1.1491493664165704, 0.8349809885931612, 0.041749049429658056, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.8750356753126798, 1.8750356753126798, 1.5245093595232055, 1.5060606060606017, 1.2801515151515115, 5.5714995278808725, 5.493863745133269, 4.795141700404834, 4.795141700404834, 4.795141700404834, 7.089951818006582, 7.089951818006581, 2.549189210249129, 0.7536491677336715, 0.7536491677336715, 0.7536491677336715, 0.0, 0.0, 0.0, 0.0, 1.4823529411764724, 0.44470588235294173, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.1319587628866, 0.8489690721649499, 0.0, 0.0, 0.0, 12.225457702676872, 2.6146356167873, 2.001384083044967, 2.001384083044967, 2.001384083044967, 2.001384083044967, 2.001384083044967, 1.0006920415224836, 0.0, 0.0, 12.38222222222225, 12.38222222222225, 12.38222222222225, 12.38222222222225, 12.38222222222225, 17.263342227803093, 7.976675561136408, 7.969719039397277, 7.837545126353799, 7.837545126353799, 7.837545126353799, 1.1756317689530695, 0.0, 0.0, 0.0, 12.846573382605058, 10.960543970840352, 10.331867500252116, 7.550293032167001, 1.0599526066350637, 2.1341542949223316, 1.5029938037547348, 0.8862665310274619, 0.8862665310274619, 0.8862665310274619, 0.8862665310274619, 0.8862665310274619, 0.5760732451678502, 0.0, 0.0, 0.9193359374999982, 0.9193359374999982, 0.9193359374999982, 0.9193359374999982, 0.22983398437499955, 1.7932075471698132, 1.7932075471698132, 1.34490566037736, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.528999999999995, 2.528999999999995, 2.528999999999995, 1.3909499999999975, 0.0, 6.058964167787706, 6.058964167787706, 6.058964167787706, 6.058964167787706, 6.058964167787706, 4.466040485024445, 2.545531740986288, 2.5455317409862874, 2.545531740986288, 1.6651471256016688, 1.2465765490893181, 0.8602247191011255, 0.8602247191011255, 0.8602247191011255, 0.8602247191011255, 3.3391174526651417, 3.3391174526651417, 1.4799479024921296, 0.04301123595505627, 0.0, 0.7115459882583155, 0.7115459882583155, 0.7115459882583155, 0.21346379647749467, 0.0, 9.348, 9.348, 9.348, 9.348, 9.348, 12.173038167938927, 12.173038167938927, 5.629438167938925, 2.825038167938925, 2.825038167938925, 1.8362748091603014, 0.0, 0.0, 0.0, 0.0, 0.9268292682926843, 0.9268292682926843, 0.9268292682926843, 0.9268292682926843, 0.7414634146341474, 12.552127659574461, 12.552127659574461, 0.6276063829787231, 0.0, 0.0, 1.2751188589540328, 1.2751188589540328, 1.2751188589540328, 1.2751188589540328, 0.25502377179080654, 4.816836734693867, 4.816836734693867, 4.816836734693867, 4.816836734693867, 4.816836734693867, 3.8475944908609403, 3.1250689806568603, 1.771743345552932, 1.5329211746522384, 0.0, 8.349601313403106, 4.938461417030046, 1.3503859174001374, 0.1228436018957348, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.1670329670329587, 1.1670329670329587, 1.1670329670329587, 1.1670329670329587, 1.1670329670329587, 5.057855478969747, 5.057855478969747, 4.357635698749972, 2.1423245518075715, 0.0, 1.023459244532805, 0.35821073558648175, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 15.895768135698487, 10.264179672504339, 10.033981652702359, 8.766684355405065, 8.223556942277652, 11.418369111802704, 3.6059900166389345, 1.0817970049916803, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.2152429616603238, 2.2152429616603238, 2.2152429616603238, 2.098877681370993, 1.0018348623853157, 1.813917525773199, 1.3604381443298992, 0.0, 0.0, 0.0, 10.74988399071929, 10.74988399071929, 10.74988399071929, 10.74988399071929, 10.74988399071929, 11.275680615084699, 1.4439320388349404, 1.4439320388349404, 1.4439320388349404, 0.9385558252427113, 9.891460815814733, 9.891460815814733, 9.891460815814733, 9.863589848072799, 9.70565436420183, 10.548777040781758, 9.916435033346813, 9.70565436420183, 9.362693199153286, 9.019732034104742, 3.15690621193666, 0.0, 0.0, 0.0, 0.0, 2.551858407079643, 2.551858407079643, 2.551858407079643, 2.296672566371679, 0.0, 23.917077986179592, 23.917077986179592, 23.917077986179592, 10.762685093780819, 0.0, 6.47799442896936, 6.47799442896936, 6.47799442896936, 6.47799442896936, 6.47799442896936, 18.91991963895629, 15.342609171459559, 10.750358944926683, 10.750358944926683, 7.334924217423524, 4.322198492684055, 4.322198492684055, 3.9706890587217907, 3.5410664172123574, 3.5410664172123574, 4.150515236109994, 3.442301952667522, 0.6094488188976364, 0.6094488188976364, 0.6094488188976364, 1.8000512285361914, 0.6094488188976364, 0.6094488188976364, 0.18283464566929092, 0.0, 4.759478057938715, 4.6494780579387145, 3.659478057938713, 3.659478057938713, 3.659478057938713, 2.1699531291041465, 0.49631067961165176, 0.49631067961165176, 0.49631067961165176, 0.49631067961165176, 0.49631067961165176, 0.49631067961165176, 0.04963106796116517, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.3557446808510658, 1.3557446808510658, 1.3557446808510658, 0.0, 0.0, 4.133330377968442, 4.133330377968442, 2.888501220793042, 0.2042514970059882, 0.0, 2.6054988352362614, 2.6054988352362614, 1.1489850737683716, 0.5247648902821331, 0.5247648902821331, 2.4415108166402733, 2.38903432761206, 2.38903432761206, 2.38903432761206, 1.3258615230794895, 1.2199368459040096, 1.2199368459040099, 1.2199368459040096, 1.2199368459040096, 1.2199368459040096, 6.762424072912611, 3.547980399277508, 2.58902524544178, 1.2893478260869493, 1.2893478260869493, 1.811330426891813, 1.682395644283118, 1.682395644283118, 1.4300362976406504, 0.0, 0.7605095541401289, 0.7605095541401289, 0.7605095541401289, 0.7605095541401289, 0.7605095541401289, 8.68307167847851, 8.302816901408447, 8.302816901408447, 1.245422535211267, 0.0, 15.959728506787265, 15.959728506787265, 15.959728506787265, 15.959728506787265, 15.959728506787265, 15.959728506787265, 13.565769230769176, 0.0, 0.0, 0.0, 12.455053941908716, 12.455053941908718, 12.455053941908718, 8.563185062240667, 0.0, 4.046966731898245, 4.046966731898245, 4.046966731898245, 3.6422700587084202, 0.0, 1.1668744434550309, 1.1668744434550309, 1.1668744434550309, 1.1668744434550309, 1.1668744434550309, 1.1668744434550309, 0.7584683882457701, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.6357798165137557, 0.6357798165137557, 0.6357798165137557, 0.6357798165137557, 0.6357798165137557, 5.748725407508132, 5.399046508425567, 5.112945590994377, 5.112945590994377, 5.112945590994377, 15.000463178038878, 12.188343102991972, 12.188343102991972, 11.879081358025529, 10.642034378159755, 11.726079321979977, 11.726079321979975, 11.726079321979975, 7.986879321979975, 2.3780793219799747, 1.4566411197327866, 1.2940343781597532, 1.2940343781597532, 1.2940343781597532, 1.2940343781597532, 1.2940343781597532, 1.0352275025278028, 0.0, 0.0, 0.0, 0.8626262626262593, 0.8626262626262593, 0.8626262626262593, 0.8626262626262593, 0.8626262626262593, 0.8626262626262593, 0.043131313131312965, 0.0, 0.0, 0.0, 21.440581000693296, 21.440581000693296, 21.440581000693296, 21.440581000693296, 21.440581000693296, 20.727370168971827, 20.601509433962157, 20.601509433962157, 20.601509433962157, 20.601509433962157, 10.300754716981078, 0.0, 0.0, 0.0, 0.0, 1.8806719038482316, 1.8806719038482314, 1.8806719038482316, 1.8806719038482314, 0.914150164717801, 2.564225568927873, 1.744108999402483, 1.2380878265703188, 0.6723735408560324, 0.1344747081712065, 5.347572815533992, 5.347572815533992, 5.347572815533992, 0.2673786407766996, 0.0, 2.846511627906981, 2.846511627906981, 0.2846511627906981, 0.0, 0.0, 4.86545643153527, 4.86545643153527, 4.865456431535269, 4.86545643153527, 2.222956431535269, 2.317765922413216, 1.663492063492055, 1.3307936507936438, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.9584905660377385, 1.9584905660377385, 0.6854716981132084, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.225454545454542, 1.225454545454542, 1.225454545454542, 1.225454545454542, 0.2450909090909084, 0.0, 0.0, 0.0, 0.0, 0.0, 1.7021052631578901, 1.7021052631578901, 1.7021052631578901, 1.7021052631578901, 1.7021052631578901, 2.493240468662814, 1.3017667844522909, 1.3017667844522909, 1.3017667844522909, 0.3254416961130727, 4.944217687074815, 4.944217687074815, 4.944217687074815, 4.944217687074815, 4.944217687074815, 6.4241010689990015, 6.4241010689990015, 6.4241010689990015, 4.446413994169075, 1.4798833819241866, 4.877692087876219, 4.359732904202753, 3.397808705952032, 3.397808705952032, 0.1250814332247551, 0.1250814332247551, 0.10006514657980409, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 6.8218324514662205, 6.8218324514662205, 6.23458245146622, 4.864332451466217, 4.199423984646999, 22.446085932442028, 21.946046639711184, 21.195987700614918, 20.557343632818306, 5.002264886755643, 2.476991150442477, 2.476991150442477, 2.476991150442477, 2.476991150442477, 1.4861946902654863, 5.011250000000038, 5.011250000000038, 5.011250000000038, 5.011250000000038, 5.011250000000038, 6.46938769870517, 6.46938769870517, 6.46938769870517, 6.46938769870517, 6.00471378566169, 2.722944763922536, 0.9690072639225225, 0.9690072639225225, 0.9690072639225225, 0.4845036319612612, 0.0, 0.0, 0.0, 0.0, 0.0, 3.110126582278484, 3.110126582278484, 3.110126582278484, 3.110126582278484, 0.777531645569621, 3.204929469370513, 3.204929469370513, 3.204929469370513, 2.3704364372618403, 0.5313559322033853, 3.674424244488104, 3.674424244488104, 2.2810704747124726, 0.0, 0.0, 10.25880456063574, 10.25880456063574, 10.25880456063574, 5.427115052401248, 0.0, 3.5446153846153896, 1.417846153846156, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 6.789295774647899, 6.789295774647899, 6.789295774647899, 6.789295774647899, 6.789295774647899, 13.449627464907284, 11.07337394378052, 6.660331690259384, 5.936184224821596, 3.7070587479048505, 6.608976621498766, 6.608976621498766, 4.497286787845339, 3.014328358208961, 3.014328358208961, 2.260746268656721, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.705109489051092, 0.705109489051092, 0.705109489051092, 0.705109489051092, 0.6345985401459828, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.2371134020618468, 1.2371134020618468, 1.2371134020618468, 1.2371134020618468, 0.5567010309278311, 0.0, 0.0, 0.0, 0.0, 0.0, 1.2259842519685067, 1.2259842519685067, 1.2259842519685067, 1.2259842519685067, 1.2259842519685067, 3.466420809147609, 3.282523171352333, 3.282523171352333, 2.130965794303155, 1.6374412041392215, 7.145808844563298, 7.145808844563298, 7.145808844563298, 6.165878774633224, 3.6837174238417383, 5.587748344370862, 5.587748344370862, 5.587748344370862, 5.587748344370862, 5.587748344370862, 1.2124756525126625, 0.9330882352941194, 0.9330882352941194, 0.27992647058823583, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.14569983136593478, 0.14569983136593478, 0.14569983136593478, 0.14569983136593478, 0.14569983136593478, 0.14569983136593478, 0.02185497470489022, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.378571428571421, 2.378571428571421, 2.378571428571421, 2.378571428571421, 1.7839285714285658, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9914285714285764, 2.9914285714285764, 2.9914285714285764, 2.9914285714285764, 2.9914285714285764, 2.8418571428571475, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.773517872711426, 4.773517872711426, 4.773517872711426, 2.2341630340017486, 0.3998310810810823, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
}

class Timer {
  val log: Logger = LoggerFactory.getLogger(getClass)

  val startMillis: MillisSinceEpoch = nowMillis
  var takenMillis: MillisSinceEpoch = 0L

  private def nowMillis: MillisSinceEpoch = SDate.now().millisSinceEpoch

  def soFarMillis: MillisSinceEpoch = nowMillis - startMillis

  def stop: MillisSinceEpoch = {
    takenMillis = nowMillis - startMillis
    takenMillis
  }

  def report(msg: String): Unit = log.warn(s"${soFarMillis}ms $msg")

  def compare(other: Timer, msg: String): Unit = {
    val otherMillis = other.soFarMillis - soFarMillis

    if (otherMillis > soFarMillis && soFarMillis > 0) log.warn(s"$msg **slower**: $otherMillis > $soFarMillis")
    else log.warn(s"$msg quicker: $otherMillis < $soFarMillis")
  }
}
