library(tidyverse)

title <- 'img'

theme_set(theme_minimal())

data <- readxl::read_xlsx('data.xlsx')

aggregate <- subset(data, select = -region) |>
    group_by(week, family) |>
    summarise(sales = sum(sales), quantity = sum(quantity))

average <- subset(data, select = -region) |>
    group_by(week, family) |>
    summarise(sales = mean(sales), quantity = mean(quantity))

average$price <- average$sales / average$quantity

p.1 <- ggplot(data = aggregate) +
    geom_line(aes(x = week, y = sales, color = family)) +
    scale_y_log10() +
    labs(title = 'Valor en ventas',
         x = 'semana',
         y = '$')

p.2 <- ggplot(data = aggregate) +
    geom_line(aes(x = week, y = quantity, color = family)) +
    scale_y_log10() +
    labs(title = 'Unidades vendidas',
         x = 'semana',
         y = 'cantidad')

p.3 <- ggplot(data = average) +
    geom_line(aes(x = week, y = price, color = family)) +
    labs(title = 'Precio promedio',
         x = 'semana',
         y = '$')

before <- average |>
    filter(week < '2018-09-01')

after <- average |>
    filter(week >= '2018-09-01')

conf.int <- function(x, alpha) {
    # unknown variance
    avg <- mean(x)
    n <- length(x)
    s.dev <- sd(x)
    se <- s.dev / sqrt(n)
    
    df = n - 1
    t.score = qt(p = alpha / 2,
                 df = df,
                 lower.tail = F)
    
    margin.error <- t.score * se
    
    lower.bound <- avg - margin.error
    upper.bound <- avg + margin.error
    
    ci <- c(lower.bound, upper.bound)
    
    return(ci)
}

families <- unique(data$family)

# confidence intervals up to 95%
before.ci <- conf.int(before$price, 0.05) # aggregate
after.ci <- conf.int(after$price, 0.05) # aggregate

cat('Aggregate - Before\n(', before.ci[1], ',', before.ci[2], ')\n')

for (i in families) {
    vector <- before |>
        filter(family == i) |>
        pull(price)
    
    ci <- conf.int(vector, 0.05)
    
    cat(i, '- Before\n(', ci[1], ',', ci[2], ')\n')
}

cat('Aggregate - After\n(', after.ci[1], ',', after.ci[2], ')\n')

for (i in families) {
    vector <- after |>
        filter(family == i) |>
        pull(price)
    
    ci <- conf.int(vector, 0.05)
    
    cat(i, '- After\n(', ci[1], ',', ci[2], ')\n')
}

plot.save <- function(title, plots) {
    for (i in 1:length(plots)) {
        ggsave(
            path = paste(here::here(), '/out', sep = ''),
            filename = paste(title, '-', i, '.png', sep = ''),
            plot = plots[[i]],
            width = 12,
            height = 8,
            bg = 'white'
        )
    }
}

plots <- list(p.1, p.2, p.3)

plot.save(title = title,
          plots = plots)
