package controllers

object EditForm {
  import play.api.data.Forms._
  import play.api.data.Form

  case class FormDataEdit(name: String, surname: String, workplace: String, phone: Long,  id_mem : Int) {}

  val form_edit = Form(
    mapping("name" -> nonEmptyText,
      "surname" -> nonEmptyText,
      "workplace" -> text,
      "phone" -> longNumber(min=0, max=999999999999L),
       "id_mem" -> number
    )(FormDataEdit.apply)(FormDataEdit.unapply)
  )

}
