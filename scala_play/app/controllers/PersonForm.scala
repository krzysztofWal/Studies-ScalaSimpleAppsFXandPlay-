package controllers

object PersonForm {
  import play.api.data.Forms._
  import play.api.data.Form

 case class FormData(name: String, surname: String, workplace: String, phone: Long) {}

  val form = Form(
    mapping("name" -> text,
      "surname" -> nonEmptyText,
      "workplace" -> text,
      "phone" -> longNumber(min=0, max=999999999999L),
    )(FormData.apply)(FormData.unapply)
  )

}
