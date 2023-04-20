package hello

import scalafx.application.JFXApp3
import javafx.event.EventHandler
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.layout.{BorderPane, HBox}
import scalafx.scene.paint._
import scalafx.scene.text.Text
import scalafx.scene.control.{ComboBox, TextField}
import scalafx.scene.text.Font
import javafx.scene.input.KeyEvent
import jdk.nashorn.internal.objects.NativeMath.round

import scala.math.pow
import scala.util.control.Breaks._

object ScalaFXHelloWorld extends JFXApp3 {
 override def main(args:Array[String]):Unit = {
  super.main(args)
 }

  val cToF :Boolean = true
  val FToc :Boolean = false
  val zlyFormat :String = "zły format danych wejściowych"

  def calculateTemp(convType :Boolean, temper :String) :String= {
    var temp = new String()
    val appropChars = "1234567890,."
    val temperArr = temper.strip.toCharArray

    var beforeSep :String = new String("")
    var afterSep :String = new String("")

    var checkResult = true
    var multiplier = 1
    var sep = false

    var beginning = 1
    if (temperArr(0) == '+') {
    } else if (temperArr(0) == '-') {
      multiplier = -1
    } else {
      beginning = 0 //na poczatku nie ma znaku w ponizszej petli iteruj od zera
    }

    /* przeiteruj po pozostałych wyrazach*/
    var el = ' '
    breakable {
      for (i <- beginning until temperArr.length) {
        el = temperArr(i)
        if (appropChars.contains(el)) {
          // przed separatorem
          if (!sep) {
            if (el.equals(',') | el.equals('.')) {
              sep = true
            } else {
              beforeSep += el
            }
          } else {  // po separatorze
            if (el.equals(',') | el.equals('.')) {
              // dwa separatory
              checkResult = false
              break()
            } else {
              afterSep += el
            }
          }
        } else { // nieodpowiedni znak
          checkResult = false
          break()
        }
      }
    }

    if (checkResult & afterSep.isEmpty & beforeSep.isEmpty) {
      checkResult = false
    }

    if (checkResult){
      var doubleTemper :Double = 0.0
      if (afterSep.nonEmpty)
        doubleTemper = beforeSep.toInt + afterSep.toDouble/pow(10, afterSep.length)
      else
        doubleTemper = beforeSep.toDouble

      doubleTemper = multiplier * doubleTemper
      /* z C do F */
      if (convType == cToF) {
        temp = BigDecimal(1.8 * doubleTemper + 32.0).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble.toString
        /* z F do C */
      } else {
        temp = BigDecimal((doubleTemper - 32)/1.8).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble.toString
      }
     // temp = doubleTemper.toString
     // temp = (afterSep.toDouble/pow(10, afterSep.length)).toString
    } else {
      temp = zlyFormat
    }
    temp
  }

   override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      //    initStyle(StageStyle.Unified)
      title = "Temperature calculator"

      scene = new Scene(500,200) {

        fill = Color.rgb(38, 38, 38)
        val celDeg_string : String = new String(Character.toChars(Integer.parseInt("2103", 16)))
        val cToF_string : String = celDeg_string + " " + new String(Character.toChars(Integer.parseInt("21fe", 16))) + " F"
        val fToC_string : String= "F " + new String(Character.toChars(Integer.parseInt("21fe", 16))) + " " + new String(Character.toChars(Integer.parseInt("2103", 16)))

        /* typy konwersji */
        val convTypeChoice = new ComboBox(List(cToF_string, fToC_string))
        convTypeChoice.value = cToF_string

        /* top */
        val title :String = "Konwersja " +
          new String(Character.toChars(Integer.parseInt("2103", 16))) + " " +
          new String(Character.toChars(Integer.parseInt("27fa", 16))) + " F"
        val programTitle = new Text(title)
        programTitle.font = Font.apply("Comic Sans MS", 18)

        /* center */
        val wprowadzWartosc = new TextField()
        wprowadzWartosc.text = "Wprowadz wartość"
        wprowadzWartosc.setMaxSize(300,40)

        val hbTop = new HBox
        hbTop.children = Seq(programTitle)
        hbTop.alignment = Pos.Center
        hbTop.padding = Insets.apply(20)

        val hbLeftCenter = new HBox
        hbLeftCenter.children = Seq(wprowadzWartosc, convTypeChoice)
        hbLeftCenter.alignment = Pos.Center
        hbLeftCenter.padding = Insets.apply(30,15,0, 30)

        val hbRightCenter = new HBox
        hbRightCenter.children = Seq(convTypeChoice)
        hbRightCenter.alignment = Pos.Center
        hbRightCenter.padding = Insets.apply(30,0,0, 15)

        val hbCenter = new HBox
        val centerText = new Text
        centerText.alignmentInParent = Pos.Center
        centerText.font = Font.apply("Comic Sans MS", 13)
        centerText.text = "Wprowadź i naciśnij\nklawisz ENTER "
        hbCenter.children = Seq(centerText, hbLeftCenter, hbRightCenter)
        hbCenter.alignment = Pos.Center


        /* bottom */
        val hbLeftBottom = new HBox
        val resultPromptPrompt = new Text("Wynik konwersji:")
        hbLeftBottom.children = Seq(resultPromptPrompt)
        hbLeftBottom.alignment = Pos.Center
        hbLeftBottom.padding = Insets.apply(15,15,15,15)

        val hbRightBottom = new HBox
        val resultPromptResult = new Text("jeszcze nie przeliczono")
        hbRightBottom.children =  Seq(resultPromptResult)
        hbRightBottom.padding = Insets.apply(15,15,15,15)

        resultPromptPrompt.font = Font.apply("Comic Sans MS", 14)
        resultPromptResult.font = Font.apply("Comic Sans MS", 14)

        val hbBottom = new HBox
        hbBottom.children = Seq(hbLeftBottom, hbRightBottom)
        hbBottom.padding = Insets.apply(30,15,15,15)
        hbBottom.alignment = Pos.TopCenter

        /* reakcja na przycisniecie przycisku */
        wprowadzWartosc.setOnKeyTyped(new EventHandler[KeyEvent]() {
          def handle(event: KeyEvent): Unit = {
            if (event.getCharacter.charAt(0).toInt == 13 && wprowadzWartosc.getText.nonEmpty) {
              var temp = new String()
              if (convTypeChoice.getValue == cToF_string) {
              //  temp = wprowadzWartosc.getText + " " + celDeg_string + " = " + calculateTemp(cToF, wprowadzWartosc.getText) + " F"
                temp = calculateTemp(cToF, wprowadzWartosc.getText)
                if (!temp.equals(zlyFormat)) {
                  temp = wprowadzWartosc.getText + " " + celDeg_string + " = " + temp + " F"
                }
              } else {
                // temp = wprowadzWartosc.getText + " F = " + calculateTemp(FToc, wprowadzWartosc.getText) + " " + celDeg_string
                temp = calculateTemp(FToc, wprowadzWartosc.getText)
                if (!temp.equals(zlyFormat)) {
                  temp = wprowadzWartosc.getText + " F = " + temp + " " + celDeg_string
                }
              }
                resultPromptResult.text = temp
              if (!temp.equals(zlyFormat)) {
                wprowadzWartosc.text = ""
              }
            }
          }
        })

        /* ostateczny layout */
        val rootPane = new BorderPane
        rootPane.top = hbTop
        rootPane.center = hbCenter
        rootPane.bottom = hbBottom

        root = rootPane

       }
    }
  }
}

