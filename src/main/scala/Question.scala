case class Question(qestionData: (
                      Int, // question id
                      String, // question header
                      QuestionType, // open | choice | multi
                      Vector[String], // answer variants
                      Long) // pollId owner poll
                   )
