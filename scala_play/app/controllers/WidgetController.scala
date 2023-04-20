package controllers

import javax.inject.Inject
import models.Person
import play.api.data._
import play.api.mvc._

import java.nio.file.{Files, NoSuchFileException, Paths, StandardOpenOption}
import java.io.File
import scala.collection._
import java.time.Instant
import java.io.PrintWriter
import scala.util.control.Breaks._

class WidgetController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc)   {
  import PersonForm._
  import DeleteForm._
  import EditForm._

  private val fileName : String = "data.txt"
  private var person_arr : mutable.ArrayBuffer[Person] = mutable.ArrayBuffer()

  // url to the widget
  private val postUrlCreate = routes.WidgetController.createPersonRecord
  private val postUrlDelete = routes.WidgetController.deletePersonRecord
  private val postUrlAddEdited = routes.WidgetController.addEditedPersonRecord

  private def myCopyToArray(fileName_ : String, person_arr_ : mutable.ArrayBuffer[Person]): Unit = {
    val temp = mutable.ArrayBuffer[Array[String]]()
    scala.io.Source.fromFile(fileName_).getLines().toList.foreach(temp += _.split("\t"))
    if (!temp.isEmpty) {
      temp.foreach(f => person_arr_ += Person(f(0), f(1), f(2), f(3).toLong, f(4).toLong))
    }
  }

  private def findInArray(searchId : Long, person_arr_ : mutable.ArrayBuffer[Person]): Int = {
    for (ind <- 0 to person_arr_.length - 1) {
      if (person_arr_(ind).id == searchId)
        return ind
    }
    return -1;
  }

  private def getIdFromUnsubmittedForm(f :Form[FormDataEdit]): Int = {
    val tempArrStr = f.toString.split(" ")
    var counter : Int = 0
    breakable {
      tempArrStr.foreach(f => {
        if (f.equals("id_mem")) {break}
        counter += 1
      })
    }
    tempArrStr(counter+2).split(',')(0).toInt
  }

  def index = Action {
    Ok(views.html.index())
  }

  def personList = Action {
    implicit request: MessagesRequest[AnyContent] =>
      Ok(views.html.personList(person_arr.toSeq, form, postUrlCreate))
  }

  def showList = Action {
    implicit  request: MessagesRequest[AnyContent] =>
      person_arr.clear()
      myCopyToArray(fileName, person_arr)
      person_arr = person_arr sortBy(_.name.toUpperCase)
      Ok(views.html.showList(person_arr.toSeq, postUrlDelete, form_))
  }

  def createPersonRecord = Action {
    implicit request: MessagesRequest[AnyContent] =>
      val inCaseOfError = {
        formWithErrors : Form[FormData] =>
          BadRequest(views.html.personList(person_arr.toSeq, formWithErrors, postUrlCreate))
      }

      val inCaseOfSuccess = { data : FormData =>
        try {
          Files.write(Paths.get(fileName), String.format("%s\t%s\t%s\t%d\t%d\n", data.name, data.surname, data.workplace, data.phone ,Instant.now.getEpochSecond).getBytes, StandardOpenOption.APPEND)
        } catch {
          case ex: NoSuchFileException => {
            new File(fileName).createNewFile()
            Files.write(Paths.get(fileName), String.format("%s\t%s\t%s\t%d\t%d\n", data.name, data.surname, data.workplace, data.phone ,Instant.now.getEpochSecond).getBytes, StandardOpenOption.APPEND)
          }
        }
          Redirect(routes.WidgetController.personList).flashing("Informacja" -> "Dodano rekord")
      }

      val formValidationResult = form.bindFromRequest()
      formValidationResult.fold(inCaseOfError, inCaseOfSuccess)

  }

  def deletePersonRecord = Action {
    implicit request: Request[AnyContent] =>
    val inCaseOfError_ = {
      formWithErrors : Form[FormData_] =>
        Redirect(routes.WidgetController.showList).flashing("Informacja" -> "Wystąpił błąd")
    }

    val inCaseOfSuccess_ = {
      data : FormData_ =>
        if (data.id_to_del != -1) {
          /* usuwanie rekordu */
          val ind = findInArray(data.id_to_del, person_arr)
          /* usuwanie z tablicy */
          person_arr.remove(ind)

          /* wyczyszczenie pliku */
          val pw = new PrintWriter(fileName)
          pw.close

          /* przepisanie tablicy do pliku */
          person_arr.foreach(f => {
            Files.write(Paths.get(fileName), String.format("%s\t%s\t%s\t%d\t%d\n", f.name, f.surname, f.workplace, f.phone , f.id).getBytes, StandardOpenOption.APPEND)
          })
          Redirect(routes.WidgetController.showList).flashing("Informacja" -> "Usunięto rekord")
        } else {
          /* edytowanie rekordu */
          Redirect(routes.WidgetController.displayEdit(data.id_to_edit))
        }
    }

    val formValidationResult = form_.bindFromRequest()
      formValidationResult.fold(inCaseOfError_, inCaseOfSuccess_)
  }

  def displayEdit(id: Int) = Action {
    implicit  request: MessagesRequest[AnyContent] =>
    Ok(views.html.editList(person_arr.toSeq, form_edit, postUrlAddEdited, person_arr(findInArray(id ,person_arr))))
  }

  def addEditedPersonRecord = Action {
    implicit request: MessagesRequest[AnyContent] =>
      val inCaseOfError = {
        formWithErrors : Form[FormDataEdit] =>
          Redirect(routes.WidgetController.displayEdit(getIdFromUnsubmittedForm(formWithErrors))).flashing("informacja" -> "Zły format danych")
      }

      val inCaseOfSuccess = {data : FormDataEdit =>

        val ind = findInArray(data.id_mem, person_arr)
        person_arr.remove(ind)

        /* wyczyszczenie pliku */
        val pw = new PrintWriter(fileName)
        pw.close

        person_arr += Person(data.name, data.surname, data.workplace, data.phone, data.id_mem)
        person_arr = person_arr sortBy(_.name.toUpperCase)

        /* przepisanie tablicy do pliku */
        person_arr.foreach(f => {
          var tempStr : String = "";
          Files.write(Paths.get(fileName), String.format("%s\t%s\t%s\t%d\t%d\n", f.name, f.surname, f.workplace, f.phone, f.id).getBytes, StandardOpenOption.APPEND)
        })
        Redirect(routes.WidgetController.showList).flashing("Informacja" -> "Zmieniono rekord")

      }
      val formValidationResult = form_edit.bindFromRequest()
      formValidationResult.fold(inCaseOfError, inCaseOfSuccess)
  }
}
