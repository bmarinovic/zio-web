package zio.web.schema

import zio.test.Assertion.equalTo
import zio.test.{ DefaultRunnableSpec, assert, suite, test }

object DeriveSchemaSpec extends DefaultRunnableSpec {

  sealed case class UserId(id: String)

  sealed trait Status
  case class Ok(response: List[String]) extends Status
  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
      extends Status
  case object Pending extends Status

  override def spec = suite("DeriveSchemaSpec")(
    test("DeriveSchema correctly derives schema for UserId case class") {

      val userIdSchema: Schema[UserId] = DeriveSchema.gen

      assert((userIdSchema: Schema[_]) match {
        case Schema.Transform(codec, _, _) =>
          (codec: Schema[_]) match {
            case Schema.Record(structure) =>
              structure("id") match {
                case Schema.Primitive(StandardType.StringType) => true
                case _                                         => false
              }
            case _ => false
          }
        case _ => false
      })(equalTo(true))
    },
    test("DeriveSchema correctly derives schema for complex ADT") {

      val statusSchema: Schema[Status] = DeriveSchema.gen[Status]

      assert((statusSchema: Schema[_]) match {
        case Schema.Transform(codec, _, _) =>
          (codec: Schema[_]) match {
            case Schema.Enumeration(enumStructure) =>
              val okBranch = enumStructure("Ok") match {
                case Schema.Transform(okSchema, _, _) =>
                  (okSchema: Schema[_]) match {
                    case Schema.Record(responseStructure) =>
                      responseStructure("response") match {
                        case Schema.Transform(responseSchema, _, _) =>
                          (responseSchema: Schema[_]) match {
                            case Schema.Sequence(elementSchema) =>
                              (elementSchema: Schema[_]) match {
                                case Schema.Primitive(StandardType.StringType) => true
                                case _                                         => false
                              }
                            case _ => false
                          }
                        case _ => false
                      }
                    case _ => false
                  }
                case _ => false
              }

              val failedBranch = enumStructure("Failed") match {
                case Schema.Transform(failedSchema, _, _) =>
                  (failedSchema: Schema[_]) match {
                    case Schema.Record(failedStructure) =>
                      val code = failedStructure("code") match {
                        case Schema.Primitive(StandardType.IntType) => true
                        case _                                      => false
                      }

                      val reason = failedStructure("reason") match {
                        case Schema.Primitive(StandardType.StringType) => true
                        case _                                         => false
                      }

                      val additionalExplanation = failedStructure("additionalExplanation") match {
                        case Schema.Optional(additionalExplanationSchema) =>
                          (additionalExplanationSchema: Schema[_]) match {
                            case Schema.Primitive(StandardType.StringType) => true
                            case _                                         => false
                          }
                        case _ => false
                      }

                      val remark = failedStructure("remark") match {
                        case Schema.Primitive(StandardType.StringType) => true
                        case _                                         => false
                      }

                      code && reason && additionalExplanation && remark

                    case _ => false
                  }
                case _ => false
              }

              val pendingBranch = enumStructure("Pending") match {
                case Schema.Transform(pendingSchema, _, _) =>
                  (pendingSchema: Schema[_]) match {
                    case Schema.Record(pendingStructure) => pendingStructure.isEmpty
                    case _                               => false
                  }
                case _ => false
              }

              okBranch && failedBranch && pendingBranch
            case _ => false
          }
        case _ => false
      })(equalTo(true))
    }
  )
}
