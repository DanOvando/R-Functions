lrsg_model <- function(dat,b0,s,z,q,rlags)
{

  time <- dim(dat)[1]

  r0 <- b0 * (1-s)

  a <- b0/r0 * (1 - (z - 0.2)/(0.8*z))

  b <- (z- 0.2)/(0.8 * z * r0)

  dat$predicted_b[1] <- b0


  for (t in 1:time)
  {
    dat$predicted_cpue[t] <- dat$predicted_b[t] * q

    if ((t - rlags)<=0){
      dat$predicted_recruitment[t] <- r0
    }else {
      dat$predicted_recruitment[t] <- dat$predicted_b[t-rlags] / (a +  b*dat$predicted_b[t-rlags])
    }
    if (t < time){
      dat$predicted_b[t+1] <- pmax(.01,s*dat$predicted_b[t] + dat$predicted_recruitment[t] - dat$catch[t])
    }

  }

  dat$lag_predicted_b <- lag(dat$predicted_b,rlags)

  dat$bmsy <- 1/b * sqrt( a / (1-s) - a)

  dat$MSY <- dat$bmsy * ( s - 1 + 1 / (a + b*dat$bmsy))

  recruit_plot <- (ggplot(dat, aes(lag_predicted_b,predicted_recruitment)) +
                     geom_line(size = 2, color = 'blue') +
                     xlab('Biomass') +
                     ylab('Recruitment'))

  bio_catch_plot <- (ggplot(dat, aes(year,catch)) +
                       geom_point(shape = 21, fill = 'red') +
                       geom_line(aes(year,predicted_b)) +
                       xlab('Year') +
                       ylab('Biomass(line)/Catch(dots)'))

  cpue_plot <- (ggplot(dat, aes(year,cpue)) +
                  geom_point(shape = 21, fill = 'blue') +
                  geom_line(aes(year,predicted_cpue)) +
                  xlab('year') +
                  ylab('cpue'))

  plots <- ls(pattern = '_plot')

  lrsg_plots <- list()

  for (f in 1:length(plots))
  {
    eval(parse(text = paste('lrsg_plots$',plots[f],'=',plots[f], sep = '')))
  }

  return(list(dat = dat,lrsg_plots = lrsg_plots))

}