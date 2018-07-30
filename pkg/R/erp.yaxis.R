# this function is basically a wrapper for axis with msectopoints embedded and with some little additional features.
# for finer tuning use directly axis (see also par).

## NOTICE!! If you call this after another axis, then you cannot use anymore the y.pos parameter. you have to use the 

erp.yaxis <- function (length.erp = NULL, startmsec=NULL, endmsec = NULL, y.tick=seq(-6,6,2), y.labels = y.tick, y.las=0, draw.y.ticks=TRUE, y.pos = NA,  y.outer = FALSE, y.font = NA, y.lty = "solid", y.lwd = 1, y.lwd.ticks = 1, y.col = NULL, y.col.ticks = NULL, y.hadj=NA, y.padj = NA, y.tcl = -0.5, y.tick.both= FALSE, y.cex = 1)
{
  
  if (!is.na(y.pos)){
    y.pos=msectopoints(y.pos, length.erp, startmsec, endmsec)
  }
  
  if (is.null(startmsec)|is.null(endmsec)){
    stop("startmsec and endmsec must be specified", call.=F)
  }
  
  
  # tick and labels positions
  
  axis(2, at= y.tick, labels=y.labels, pos= y.pos, tick = draw.y.ticks, outer = y.outer, font = y.font, lty = y.lty, lwd = y.lwd, lwd.ticks = y.lwd.ticks, col = y.col, col.ticks = y.col.ticks, padj = y.padj, tcl = y.tcl, cex.axis = y.cex, las=y.las)
  
  if (y.tick.both==TRUE){
    axis(2, at= y.tick, labels= FALSE, pos= y.pos, tick = draw.y.ticks, outer = y.outer, font = y.font, lty = y.lty, lwd = y.lwd, lwd.ticks = y.lwd.ticks, col = y.col, col.ticks = y.col.ticks, hadj = y.hadj, padj = y.padj, tcl = - y.tcl, las=y.las) # NOTICE the minus: it is for plotting in the other direction
    
  }
  
  
}