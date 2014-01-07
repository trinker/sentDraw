#' Title
#' 
#' Description
#' 
#' @param dataframe
#' @param len.col
#' @param turn
#' @return
#' @references
#' @keywords
#' @export
#' @seealso
#' @examples
turn_it <- function(dataframe, len.col, turn = -pi/2) {

    dat <- dataframe
    dat[, "turn"] <- rep(turn, nrow(dataframe))
    dat <- within(dat, { 
        facing <- pi/2 + cumsum(turn)
        move <- dat[, len.col] * exp(1i * facing)
        position <- cumsum(move)
        x2 <- Re(position)
        y2 <- Im(position)
        x1 <- c(0, head(x2, -1))
        y1 <- c(0, head(y2, -1))
    })

    dat[, c("x1", "y1", "x2", "y2")] <- lapply(dat[, c("x1", "y1", "x2", "y2")], 
    	round, digits=0)
    data.frame(dataframe, dat[, c("x1", "y1", "x2", "y2")])
}

