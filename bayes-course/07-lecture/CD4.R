pct_chg <- function(x) {
  c(0, diff(x)/x[1:(length(x) - 1)])
}

d <- ALA::cd4 |>
  group_by(id) |>
  mutate(d_log = c(0, diff(logCD4)),
         pct   = pct_chg(exp(logCD4)),
         n = n()) |>
  ungroup()

p <- ggplot(aes(week, logCD4, color = treatment, group_by = id), data = d)
p + geom_line(linewidth = 0.2, alpha = 1/3) +  facet_wrap(vars(treatment))

p <- ggplot(aes(week, d_log, color = treatment, group_by = id), data = d)
p + geom_line(linewidth = 0.2, alpha = 1/3) +  facet_wrap(vars(treatment))

p <- ggplot(aes(week, pct, color = treatment, group_by = id), data = d)
p + geom_line(linewidth = 0.2, alpha = 1/3) +  facet_wrap(vars(treatment))

p <- ggplot(aes(week, pct, color = treatment, group_by = id), data = d)
p + geom_line(linewidth = 0.2, alpha = 1/3) +  facet_wrap(vars(treatment)) +
  ylim(-1, 20)

# select 25 patients at random
dd <- d |> group_split(id) |> sample(36) |> bind_rows()
p <- ggplot(aes(week, logCD4, color = treatment), data = dd)
p + geom_point(size = 0.5) + geom_line() +  
  facet_wrap(vars(id), scales = "free_y")

x <- seq(1, 1.05, len = 100)
pct_x <- seq(0, 0.05, len = 100)
d <- data.frame(x, pct_x, log_x = log(x))
p <- ggplot(aes(x, log_x), data = d)
p + geom_line() + geom_line(aes(y = pct_x), color = 'red', linewidth = 0.2)
