#Practicing different types of tables

ft <- flextable(airquality[ sample.int(10),])
ft <- add_header_row(ft,
										 colwidths = c(4, 2),
										 values = c("Air quality", "Time")
)
ft <- theme_vanilla(ft)
ft <- add_footer_lines(ft, "Daily air quality measurements in New York, May to September 1973.")
ft <- color(ft, part = "footer", color = "#666666")
ft <- set_caption(ft, caption = "New York Air Quality Measurements")
ft

#defining the formatting for your flextables
set_flextable_defaults(
	font.size = 10, theme_fun = theme_vanilla,
	padding = 6,
	background.color = "#EFEFEF")
flextable(cars)


#1.4: Walkthrough: simple example
myft <- flextable(head(mtcars),
									col_keys = c("am", "carb", "gear", "mpg", "drat" ))
myft

myft <- fontsize(mfyt, size=20)
myft


