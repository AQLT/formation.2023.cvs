library(rjd3highfreq)
library(forecast)
library(plotly)
library(ggplot2)
trace_ch <- function(x, p0, p1, np= p1 - p0 + 1, original = TRUE, interactive = FALSE) {
	sa_tests <- rjd3toolkit::seasonality_canovahansen(
		x,
		p0 = p0, p1 = p1, np = np,
		original = original)
	x = seq.int(from = p0, length.out = length(sa_tests))
	if (interactive) {
		plot_ly() |>
			add_trace(
				type = "scatter",
				mode = "lines",
				x = x,
				y = sa_tests
			)
	} else {
		plot(x, sa_tests, type = "l")
	}
}
graph_comp <- function(data) {
	dataGraph <- reshape2::melt(data, id="x")
	ggplot(data = dataGraph, aes(x = x, y = value, color = variable)) +
		geom_line()
}
x = seq(as.POSIXct("2020-06-05 00:00:00"), as.POSIXct("2020-08-27 23:30:00"), by="30 min")
length(taylor)
tail(seq(as.POSIXct("2020-06-05 00:00:00"), as.POSIXct("2020-08-27 24:00:00"), by="30 min"))
plot(taylor)
trace_ch(taylor, p0 = 2, p1 = 2*24, interactive = T)
trace_ch(taylor, p0 = 2, p1 = 2*24 * 7, interactive = T)
plot(diff(diff(taylor, 2*24), 2*24*7))
plot(rjd3toolkit::differencing_fast(diff(taylor, 2*24), 2*24*7))
trace_ch(diff(diff(taylor, 2*24), 2*24*7), p0 = 2, p1 = 2*24 * 7, interactive = T)
rjd3toolkit::differencing_fast(taylor)$ddata |> plot()
plot(rjd3toolkit::differences(taylor, c(1, 2*24*7)))

# On identifie essentiellement 3 saisonnalités :
# journalière (48), toutes les 12 h et toutes les 6 h (12 et 24)
# Ces saisonnalités se cumulent
plot_ly() |>
	add_trace(
		type = "scatter",
		mode = "lines",
		x = seq.int(from = 2, length.out = length(sa_tests)),
		y = sa_tests
	)

mstl_mod <- mstl(taylor)
autoplot(mstl_mod)
tbats_mod <- tbats(taylor, seasonal.periods = c(2 * 24, 2*24*7))

pre_pro <- fractionalAirlineEstimation(
	y = taylor,
	periods = c(2*24*7),
	outliers = c("ao", "wo"),
	log = FALSE, y_time = x)
# Juste un point atypique
pre_pro
plot(pre_pro)

amb.p1 <- rjd3highfreq::fractionalAirlineDecomposition(
	y = pre_pro$model$linearized, # linearized series from preprocessing
	period = 2 * 24,
	log = FALSE)
# Il reste de la saisonnalité
plot(ggdemetra3::seasonaladj(amb.p1), type = "l")

# On ajuste maintenant la saisonnalité hebdomadaire :
amb.p2 <- rjd3highfreq::fractionalAirlineDecomposition(
	y = amb.p1$decomposition$sa,
	period = 2*24*7,
	log = FALSE,)
plot(ggdemetra3::seasonaladj(amb.p2), type = "l")

# Si on estime directement la saisonnalité hebdo le résultat semble proche
amb.sem <- rjd3highfreq::fractionalAirlineDecomposition(
	y = pre_pro$model$linearized,
	period = 2*24*7,
	log = FALSE)
plot(ggdemetra3::seasonaladj(amb.p2), type = "l")
lines(ggdemetra3::seasonaladj(amb.sem), col = "red")



amb.multi <- rjd3highfreq::multiAirlineDecomposition(
	y = pre_pro$model$linearized, # input time series
	periods = c(2 * 24, 2*24*7), # 2 frequency
	log = FALSE, ndiff = 2)
plot(ggdemetra3::seasonaladj(amb.multi), type = "l")

data_sa <- data.frame(mstl = c(forecast::seasadj(mstl_mod)),
					  # tbats = c(forecast::seasadj(tbats_mod)),
					  amb_sem = ggdemetra3::seasonaladj(amb.sem),
					  amb_2step = ggdemetra3::seasonaladj(amb.p2),
					  amb_multi = ggdemetra3::seasonaladj(amb.multi))
data_t <- data.frame(mstl = c(forecast::trendcycle(mstl_mod)),
					 amb_sem = ggdemetra3::trendcycle(amb.sem),
					 amb_2step = ggdemetra3::trendcycle(amb.p2),
					 amb_multi = ggdemetra3::trendcycle(amb.multi))
data_i <- data.frame(mstl = c(forecast::remainder(mstl_mod)),
					 amb_sem = ggdemetra3::irregular(amb.sem),
					 amb_2step = ggdemetra3::irregular(amb.p2),
					 amb_multi = ggdemetra3::irregular(amb.multi))
data_i <- data.frame(mstl = c(forecast::seasonal(mstl_mod)[]),
					 amb_sem = ggdemetra3::irregular(amb.sem),
					 amb_2step = ggdemetra3::irregular(amb.p2),
					 amb_multi = ggdemetra3::seasonal(amb.multi))
colnames(mstl_mod)
c(mstl_mod[, "Seasonal48"] + mstl_mod[, "Seasonal336"])

graph_comp(data_sa)
graph_comp(data_t)
graph_comp(data_i)

plot(c(forecast::seasadj(mstl_mod)), type = "l")
lines(ggdemetra3::seasonaladj(amb.multi), col = "red")
lines(ggdemetra3::seasonaladj(amb.p2), col = "green")

plot(c(forecast::trendcycle(mstl_mod)), type = "l")
lines(ggdemetra3::trendcycle(amb.multi), col = "red")

lines(amb.multi$decomposition$s_48, col = "red")
lines(ggdemetra3::seasonaladj(amb.multi), col = "red")
View(amb.multi$decomposition)
plot(amb.multi)

