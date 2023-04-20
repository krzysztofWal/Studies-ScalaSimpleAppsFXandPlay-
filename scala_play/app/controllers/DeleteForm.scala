package controllers

object DeleteForm {
  import play.api.data.Forms._
  import play.api.data.Form

  case class FormData_(id_to_del: Int, id_to_edit: Int) {}

  val form_ = Form(
    mapping(
      "id_to_del" -> number,
      "id_to_edit" -> number
    )(FormData_.apply)(FormData_.unapply)
  )

}
