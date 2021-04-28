test_that("local decide title", {


  expect_equal(
    local_decide_title(NULL,
                       data.frame(`市名` = c("枚方市"))), "枚方市")

  expect_equal(
    local_decide_title(NULL,
                       data.frame(`市名` = c("枚方市","寝屋川市"))), "枚方市等")

  expect_equal(
    local_decide_title("交野市",
                       data.frame(`市名` = c("枚方市","寝屋川市"))), "交野市")

})

test_that("local make filename ", {

  expect_equal(
    local_output_filename("hoge", "枚方市","（住宅地）"),"hoge.pdf")

  expect_match(
    local_output_filename("", "枚方市","（住宅地）"),"枚方市（住宅地）\\d{8}\\.pdf")

})
