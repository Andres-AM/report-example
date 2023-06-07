 
#' @author Andrés Acuña Marroquín
#' @title Model Interpolation
#' @param data is a data frame
#' @param xaxis Measurement values of reference method (string parameter).
#' @param yaxis Measurement values of test method (string parameter).
#' @param value is the x value to interpolate
#' @param n_grid is a numeric parameter for seq_range
#' @return A list containing a plot showing the regressions (for all models and each models), model summary and y interpolated value
#' @export

get_prediction <-  function(data = data, xaxis = xaxis, yaxis = yaxis, value = value, n_grid = 10000) {
  
  data <- as.data.frame(data)
  
  x_axis <- data[xaxis][[1]]
  y_axis <- data[yaxis][[1]]
  
  # Run models (order 1, 2 and 3)
  ord.1 <- lm(y_axis ~ poly(x_axis, 1, raw = T)) ; eq.1 = get_model(ord.1) ;sum.ord1 <- summary(ord.1)
  ord.2 <- lm(y_axis ~ poly(x_axis, 2, raw = T)) ; eq.2 = get_model(ord.2) ;sum.ord2 <- summary(ord.2)
  ord.3 <- lm(y_axis ~ poly(x_axis, 3, raw = T)) ; eq.3 = get_model(ord.3) ;sum.ord3 <- summary(ord.3)
  
  # r squared
  r.squared <- tibble(model =c("ord.1","ord.2","ord.3"), r.squared = c(sum.ord1$r.squared,  sum.ord2$r.squared , sum.ord3$r.squared ))
  
  # grid of three models for y predictions
  grid <- data %>%
    modelr::data_grid( x_axis = modelr::seq_range(x_axis, n= n_grid)) %>%
    modelr::gather_predictions(ord.1, ord.2, ord.3,.pred = "y_axis")
  
  # table of y predicted value for 3 models
  pred_val <- data %>%
    modelr::data_grid( x_axis = value) %>%
    modelr::gather_predictions(ord.1, ord.2, ord.3,.pred = "y_axis") %>%
    left_join(r.squared,by = "model")
  
  # plot of regression for 3 models
  plot <- ggplot(data, aes(x_axis, y_axis)) +
    geom_point() +
    geom_line(data = grid ,aes(color = model)) +
    theme(legend.position = "bottom")
  
  # plot of regression for model order 1 (linear model)
  plot.ord1 <- ggplot(data, aes(x_axis,y_axis)) +
    geom_point() +
    geom_line(data = grid %>% filter(model == "ord.1") ,color = "red") +
    theme(legend.position = "bottom") + labs(title = paste("model ord1",", r.squared =",sum.ord1$r.squared))
  
  # plot of regression for model order 2
  plot.ord2 <- ggplot(data, aes(x_axis,y_axis)) +
    geom_point() +
    geom_line(data = grid %>% filter(model == "ord.2") ,color = "green") +
    theme(legend.position = "bottom") + labs(title = paste("model ord2",", r.squared =",sum.ord2$r.squared))
  
  # plot of regression for model order 3
  plot.ord3 <- ggplot(data, aes(x_axis,y_axis)) +
    geom_point() +
    geom_line(data = grid %>% filter(model == "ord.3") ,color = "blue") +
    theme(legend.position = "bottom") + labs(title = paste("model ord3",", r.squared =",sum.ord3$r.squared))
  
  results <- list(plot = plot, pred_val = pred_val,
                  plot.ord.1 =  plot.ord1, plot.ord.2 = plot.ord2, plot.ord.3 = plot.ord3,
                  sum.ord1 =  sum.ord1,
                  sum.ord2 =  sum.ord2,
                  sum.ord3 =  sum.ord3,
                  eq.1 = eq.1,
                  eq.2 = eq.2,
                  eq.3 = eq.3
  )
  return(results)
}


