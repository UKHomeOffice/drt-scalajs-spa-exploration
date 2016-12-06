package spatutorial.client.components

import diode.data.{Pot, Ready}
import diode.react._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.vdom.svg.{all => s}
import org.scalajs.dom.svg.{G, Text}
import spatutorial.client.components.Heatmap.Series
import spatutorial.client.logger._
import spatutorial.client.modules.Dashboard
import spatutorial.client.services.HandyStuff.QueueUserDeskRecs
import spatutorial.client.services._
import spatutorial.shared.FlightsApi.{QueueName, QueueWorkloads}
import spatutorial.shared._

import scala.collection.immutable.{IndexedSeq, Map, Seq}
import scala.util.{Failure, Success, Try}


object TerminalHeatmaps {
  def heatmapOfWorkloads() = {
    val workloadsRCP = SPACircuit.connect(_.workload)
    workloadsRCP((workloadsMP: ModelProxy[Pot[Workloads]]) => {

      <.div(
      workloadsMP().renderReady(wl => {
        val heatMapSeries = workloads(wl.workloads("T1"))
        val maxAcrossAllSeries = heatMapSeries.map(x => emptySafeMax(x.data)).max
        log.info(s"Got max workload of ${maxAcrossAllSeries}")
        <.div(
          <.h4("heatmap of workloads"),
          Heatmap.heatmap(Heatmap.Props(series = heatMapSeries, height = 200,
            scaleFunction = Heatmap.bucketScale(maxAcrossAllSeries)))
        )
        }))
    })

  }

  def emptySafeMax(data: Seq[Double]): Double = {
    data match {
      case Nil =>
        0d
      case d =>
        d.max
    }
  }

  def heatmapOfWaittimes() = {
    val seriiRCP: ReactConnectProxy[List[Series]] = SPACircuit.connect(waitTimes(_))
    seriiRCP((serMP: ModelProxy[List[Series]]) => {
      val p: List[Series] = serMP()
      p match {
        case Nil =>
          <.h4("No waittimes in simulation yet")
        case waitTimes =>
          val maxAcrossAllSeries = emptySafeMax(p.map(x => x.data.max))
          log.info(s"Got max waittime of ${maxAcrossAllSeries}")
          <.div(
            <.h4("heatmap of wait times"),
            Heatmap.heatmap(Heatmap.Props(series = p, height = 200,
              scaleFunction = Heatmap.bucketScale(maxAcrossAllSeries)))
          )
      }
    })
  }

  def heatmapOfDeskRecs() = {

    val seriiRCP: ReactConnectProxy[List[Series]] = SPACircuit.connect(_.queueCrunchResults.flatMap {
      case (terminalName, queueCrunchResult) =>
        queueCrunchResult.collect {
          case (queueName, Ready((Ready(crunchResult), _))) =>
            val series = Heatmap.seriesFromCrunchResult(crunchResult)
            Series(terminalName + "/" + queueName, series.map(_.toDouble))
        }
    }.toList)
    seriiRCP((serMP: ModelProxy[List[Series]]) => {
      <.div(
        <.h4("heatmap of desk recommendations"),
        Heatmap.heatmap(Heatmap.Props(series = serMP(), height = 200, scaleFunction = Heatmap.bucketScale(20)))
      )
    })
  }


  def heatmapOfDeskRecsVsActualDesks() = {
    val seriiRCP = SPACircuit.connect {
      deskRecsVsActualDesks(_)
    }

    seriiRCP((serMP: ModelProxy[List[Series]]) => {
      val p: List[Series] = serMP()
      val maxRatioAcrossAllSeries = emptySafeMax(p.map(_.data.max)) + 1
      <.div(
        <.h4("heatmap of ratio of desk rec to actual desks"),
        Heatmap.heatmap(Heatmap.Props(series = p, height = 200,
          scaleFunction = Heatmap.bucketScale(maxRatioAcrossAllSeries))))
    })
  }

  def workloads(terminalWorkloads: Map[QueueName, QueueWorkloads]): List[Series] = {
    val terminalName = "T1"
    log.info(s"!!!!looking up $terminalName in wls")
    val queueWorkloads: Predef.Map[String, List[Double]] = Dashboard.chartDataFromWorkloads(terminalWorkloads, 60)
    val result: Iterable[Series] = for {
      (queue, work) <- queueWorkloads
    } yield {
      Series(terminalName + "/" + queue, work.toVector)
    }
    result.toList
  }

  def waitTimes(rootModel: RootModel): List[Series] = {
    //      Series("T1/eeadesk", Vector(2)) :: Nil
    val terminalName = "T1"

    log.info(s"simulation results ${rootModel.simulationResult}")
    val terminalQueues = rootModel.simulationResult.getOrElse(terminalName, Map())
    val result: Iterable[Series] = for {
      queueName <- terminalQueues.keys
      simResult <- rootModel.simulationResult(terminalName)(queueName)
      waitTimes = simResult.waitTimes
    } yield {
      Series(terminalName + "/" + queueName,
        waitTimes.grouped(60).map(_.max.toDouble).toVector)
    }
    val resultList = result.toList
    log.info(s"gotSimResults ${resultList}")
    resultList
  }

  def deskRecsVsActualDesks(rootModel: RootModel): List[Series] = {
    //      Series("T1/eeadesk", Vector(2)) :: Nil
    val terminalName = "T1"
    log.info(s"deskRecsVsActualDesks")
    val result: Iterable[Series] = for {
      queueCrunchResults <- rootModel.queueCrunchResults.get(terminalName).toSeq
      queueName: QueueName <- queueCrunchResults.keys
      queueCrunchPot: Pot[(Pot[CrunchResult], Pot[UserDeskRecs])] <- queueCrunchResults.get(queueName)
      queueCrunch <- queueCrunchPot.toOption
      queueUserDeskRecs: QueueUserDeskRecs <- rootModel.userDeskRec.get(terminalName)
      userDesksPot <- queueUserDeskRecs.get(queueName)
      userDesks <- userDesksPot.toOption
      recDesksPair <- queueCrunch._1.toOption
      recDesks = recDesksPair.recommendedDesks
      userDesksVals = userDesks.items.map(_.deskRec)
    } yield {
      log.info(s"UserDeskRecs Length: ${userDesks.items.length}")
      val recDesks15MinBlocks = recDesks.grouped(15).map(_.max).toList
      val zippedRecAndUserBy15Mins = recDesks15MinBlocks.zip(userDesksVals)
      //      val zippedRecAndUser = maxRecDesks.zip(minUserDesks).map(x => x._1.toDouble / x._2)
      val ratioRecToUserPerHour = zippedRecAndUserBy15Mins.grouped(4).map(
        x => x.map(y => y._1.toDouble / y._2).max)
      Series(terminalName + "/" + queueName, ratioRecToUserPerHour.toVector)
    }
    log.info(s"gotDeskRecsvsAct")
    result.toList
  }
}

object Heatmap {

  def bucketScaleDev(n: Int, d: Int): Float = n.toFloat / d

  case class Series(name: String, data: IndexedSeq[Double])

  case class Props(width: Int = 960, height: Int, numberOfBlocks: Int = 24,
                   series: Seq[Series],
                   scaleFunction: (Double) => Int,
                   shouldShowRectValue: Boolean = true
                  )

  val colors = Vector("#D3F8E6", "#BEF4CC", "#A9F1AB", "#A8EE96", "#B2EA82", "#C3E76F", "#DCE45D",
    "#E0C54B", "#DD983A", "#DA6429", "#D72A18")

  def seriesFromCrunchAndUserDesk(crunchDeskAndUserDesk: IndexedSeq[(Int, DeskRecTimeslot)], blockSize: Int = 60): Vector[Double] = {
    val maxRecVsMinUser = crunchDeskAndUserDesk.grouped(blockSize).map(x => (x.map(_._1).max, x.map(_._2.deskRec).min))
    val ratios = maxRecVsMinUser.map(x => x._1.toDouble / x._2.toDouble).toVector
    ratios
  }

  def seriesFromCrunchResult(crunchResult: CrunchResult, blockSize: Int = 60) = {
    val maxRecDesksPerPeriod = crunchResult.recommendedDesks.grouped(blockSize).map(_.max).toVector
    maxRecDesksPerPeriod
  }

  def bucketScale(maxValue: Double)(bucketValue: Double): Int = {
    //    log.info(s"bucketScale: ($bucketValue * ${bucketScaleDev(colors.length - 1, maxValue.toInt)}")
    (bucketValue * bucketScaleDev(colors.length - 1, maxValue.toInt)).toInt
  }

  case class RectProps(serie: Series, numberofblocks: Int, gridSize: Int, props: Props, sIndex: Int)

  def HeatmapRects = ReactComponentB[RectProps]("HeatmapRectsSeries")
    .render_P(props =>
      s.g(^.key := props.serie.name + "-" + props.sIndex, getRects(props.serie, props.numberofblocks, props.gridSize, props.props, props.sIndex))
    ).build

  def getRects(serie: Series, numberofblocks: Int, gridSize: Int, props: Props, sIndex: Int): vdom.ReactTagOf[G] = {
    log.info(s"Rendering rects for $serie")
    Try {
      val rects = serie.data.zipWithIndex.map {
        case (periodValue, idx) => {
          val colorBucket = props.scaleFunction(periodValue)
          //Math.floor((periodValue * bucketScale)).toInt
          //          log.info(s"${serie.name} periodValue ${periodValue}, colorBucket: ${colorBucket} / ${colors.length}")
          val clippedColorBucket = Math.min(colors.length - 1, colorBucket)
          val colors1 = colors(clippedColorBucket)
          val halfGrid: Int = gridSize / 2
          s.g(
            ^.key := s"rect-${serie.name}-$idx",
            s.rect(
              ^.key := s"${serie.name}-$idx",
              s.stroke := "black",
              s.strokeWidth := "1px",
              s.x := idx * gridSize,
              s.y := sIndex * gridSize,
              s.rx := 4,
              s.ry := 4,
              s.width := gridSize,
              s.height := gridSize,
              s.fill := colors1),
            if (props.shouldShowRectValue)
              s.text(f"${periodValue}%2.1f",
                s.x := idx * gridSize, s.y := sIndex * gridSize,
                s.transform := s"translate(${halfGrid}, ${halfGrid})", s.textAnchor := "middle")
            else null
          )
        }
      }
      val label: vdom.ReactTagOf[Text] = s.text(
        ^.key := s"${serie.name}",
        serie.name,
        s.x := 0,
        s.y := sIndex * gridSize,
        s.transform := s"translate(-100, ${gridSize / 1.5})", s.textAnchor := "middle")

      val allObj = s.g(label, rects)
      allObj
    } match {
      case Failure(e) =>
        log.error("failure in heatmap: " + e.toString)
        throw e
      case Success(success) =>
        success
    }
  }

  val heatmap = ReactComponentB[Props]("Heatmap")
    .renderP((_, props) => {
      try {
        log.info(s"!!!! rendering heatmap")
        val margin = 200
        val componentWidth = props.width + margin
        val mult = 1
        val numberofblocks: Int = props.numberOfBlocks * mult
        val size: Int = 60 / mult
        val gridSize = (componentWidth - margin) / numberofblocks

        val rectsAndLabels = props.series.zipWithIndex.map { case (serie, sIndex) =>
          HeatmapRects(RectProps(serie, numberofblocks, gridSize, props, sIndex))
        }

        val hours: IndexedSeq[vdom.ReactTagOf[Text]] = (0 until 24).map(x =>
          s.text(^.key := s"bucket-$x", f"${x.toInt}%02d", s.x := x * gridSize, s.y := 0,
            s.transform := s"translate(${gridSize / 2}, -6)"))

        <.div(
          s.svg(
            ^.key := "heatmap",
            s.height := props.height,
            s.width := componentWidth, s.g(s.transform := "translate(200, 50)", rectsAndLabels.toList, hours.toList)))
      } catch {
        case e: Exception =>
          log.error("Issue in heatmap", e)
          throw e
      }
    }).build
}

