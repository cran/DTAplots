Forest <-
function (Data, study = NULL, level = 0.95, conf.int = "Wilson",
    se.axis = NULL, sp.axis = NULL, save_plot = NULL,
    res = 90, mean = NULL, digits = 2)
{
    op <- par('mar')
    on.exit(par(op))
    TP = Data$TP
    TN = Data$TN
    FP = Data$FP
    FN = Data$FN
    pos = TP + FN
    neg = FP + TN
    N = length(TP)
    z = qnorm((1 - level)/2, lower.tail = FALSE)
    p_pos = round(TP/pos, digits)
    p_neg = round(TN/neg, digits)
    if (conf.int == "Normal.approx") {
        p_pos_lo = round(p_pos - z * sqrt((p_pos * (1 - p_pos))/pos),
            digits)
        p_pos_up = round(p_pos + z * sqrt((p_pos * (1 - p_pos))/pos),
            digits)
        p_neg_lo = round(p_neg - z * sqrt((p_neg * (1 - p_neg))/neg),
            digits)
        p_neg_up = round(p_neg + z * sqrt((p_neg * (1 - p_neg))/neg),
            digits)
    }
    else {
        if (conf.int == "Wilson") {
            p_pos_lo = round(((p_pos + z^2/(2 * pos)) - z * sqrt((p_pos *
                (1 - p_pos)/pos) + z^2/(4 * pos^2)))/(1 + z^2/pos),
                digits)
            p_pos_up = round(((p_pos + z^2/(2 * pos)) + z * sqrt((p_pos *
                (1 - p_pos)/pos) + z^2/(4 * pos^2)))/(1 + z^2/pos),
                digits)
            p_neg_lo = round(((p_neg + z^2/(2 * neg)) - z * sqrt((p_neg *
                (1 - p_neg)/neg) + z^2/(4 * neg^2)))/(1 + z^2/neg),
                digits)
            p_neg_up = round(((p_neg + z^2/(2 * neg)) + z * sqrt((p_neg *
                (1 - p_neg)/neg) + z^2/(4 * neg^2)))/(1 + z^2/neg),
                digits)
        }
        else {
            if (conf.int == "") {
            }
        }
    }
    prop_pos = numeric()
    for (i in 1:(N)) {
        prop_pos = c(prop_pos, paste(sprintf(paste("%0.", digits,
            "f", sep = ""), p_pos[i]), " (", sprintf(paste("%0.",
            digits, "f", sep = ""), p_pos_lo[i]), "-", sprintf(paste("%0.",
            digits, "f", sep = ""), p_pos_up[i]), ")", sep = ""))
    }
    prop_neg = numeric()
    for (i in 1:(N)) {
        prop_neg = c(prop_neg, paste(sprintf(paste("%0.", digits,
            "f", sep = ""), p_neg[i]), " (", sprintf(paste("%0.",
            digits, "f", sep = ""), p_neg_lo[i]), "-", sprintf(paste("%0.",
            digits, "f", sep = ""), p_neg_up[i]), ")", sep = ""))
    }
    if (is.null(study)) {
        authors = character()
        for (i in 1:N) {
            authors = c(authors, paste("Study ", i, "", sep = ""))
        }
    }
    else {
        authors = study
    }
    if (is.null(se.axis)) {
        low_axis_se = min(p_pos_lo)
        up_axis_se = max(p_pos_up)
        cte_se = (up_axis_se - low_axis_se)
        axis_label_se = cte_se/5
    }
    else {
        low_axis_se = se.axis[1]
        up_axis_se = se.axis[2]
        cte_se = (up_axis_se - low_axis_se)
        axis_label_se = cte_se/5
    }
    if (is.null(sp.axis)) {
        low_axis_sp = min(p_neg_lo)
        up_axis_sp = max(p_neg_up)
        cte_sp = (up_axis_sp - low_axis_sp)
        axis_label_sp = cte_sp/5
    }
    else {
        low_axis_sp = sp.axis[1]
        up_axis_sp = sp.axis[2]
        cte_sp = (up_axis_sp - low_axis_sp)
        axis_label_sp = cte_sp/5
    }
    n.char = max(nchar(authors))
    {
        if (n.char <= 15) {
            X = 0
        }
        else {
            X = round(n.char/10,  0)
        }
    }
    x.1 = 0
    x.2 = 3.5 + X
    x.3 = 4.5 + X
    x.4 = 5.5 + X
    x.5 = 6.5 + X
    x.6 = 7.5 + X
    x.7 = 11 + X
    x.8 = 14.5 + X
    x.9 = 18 + X
    x.10 = 21.5 + X
    if (is.null(save_plot) == FALSE) {
        jpeg(paste(save_plot,".jpeg", sep = ""), width = 30 +
            X, height = N + 2.5, units = "cm", res = res)
    }
    if (is.null(mean)) {
        y.2 <- N + 1.5
        range = 1:N
        counter = 1
        L = 1
    }
    else {
        y.2 <- N + 3.5
        range = 2:(N + 1)
        counter = 1
        L = 2
    }
    y.1 <- 0
    default.x = range(x.1, x.10)
    default.y = range(y.1, y.2)
    par(mar = c(2.1, 0.1, 0.1, 0.1))
    plot(x = default.x, y = default.y, type = "n", xlim = range(default.x),
        ylim = range(default.y), xlab = "", ylab = "", xaxs = "i",
        yaxs = "i", axes = FALSE, frame.plot = FALSE)
    for (i in range) {
        text(x = x.1, y = i, label = authors[(N + 1) - counter],
            pos = 4, cex = 1.25)
        text(x = x.2, y = i, label = TP[(N + 1) - counter], pos = 4,
            cex = 1.25)
        text(x = x.3, y = i, label = FP[(N + 1) - counter], pos = 4,
            cex = 1.25)
        text(x = x.4, y = i, label = FN[(N + 1) - counter], pos = 4,
            cex = 1.25)
        text(x = x.5, y = i, label = TN[(N + 1) - counter], pos = 4,
            cex = 1.25)
        text(x = x.6, y = i, label = prop_pos[(N + 1) - counter],
            pos = 4, cex = 1.25)
        text(x = x.7, y = i, label = prop_neg[(N + 1) - counter],
            pos = 4, cex = 1.25)
        p_pos_lo_TRANS = (x.8) + ((x.9 - 0.5) - (x.8)) * (p_pos_lo[(N +
            1) - counter] - low_axis_se)/cte_se
        p_pos_hi_TRANS = (x.8) + ((x.9 - 0.5) - (x.8)) * (p_pos_up[(N +
            1) - counter] - low_axis_se)/cte_se
        p_pos_TRANS = (x.8) + ((x.9 - 0.5) - (x.8)) * (p_pos[(N +
            1) - counter] - low_axis_se)/cte_se
        lines(x = c(p_pos_lo_TRANS, p_pos_hi_TRANS), y = c(i,
            i))
        points(x = p_pos_TRANS, y = i, pch = 15, cex = 1.5)
        p_neg_lo_TRANS = (x.9) + ((x.10 - 0.5) - (x.9)) * (p_neg_lo[(N +
            1) - counter] - low_axis_sp)/cte_sp
        p_neg_up_TRANS = (x.9) + ((x.10 - 0.5) - (x.9)) * (p_neg_up[(N +
            1) - counter] - low_axis_sp)/cte_sp
        p_neg_TRANS = (x.9) + ((x.10 - 0.5) - (x.9)) * (p_neg[(N +
            1) - counter] - low_axis_sp)/cte_sp
        lines(x = c(p_neg_lo_TRANS, p_neg_up_TRANS), y = c(i,
            i))
        points(x = p_neg_TRANS, y = i, pch = 15, cex = 1.5)
        counter = counter + 1
    }
    text(x = x.1, y = N + L, label = "Study", pos = 4, cex = 1.25)
    text(x = x.2, y = N + L, label = "TP", pos = 4, cex = 1.25)
    text(x = x.3, y = N + L, label = "FP", pos = 4, cex = 1.25)
    text(x = x.4, y = N + L, label = "FN", pos = 4, cex = 1.25)
    text(x = x.5, y = N + L, label = "TN", pos = 4, cex = 1.25)
    text(x = x.6, y = N + L, label = paste("Sens (", round(level *
        100, 0), "% CI)", sep = ""), pos = 4, cex = 1.25)
    text(x = x.7, y = N + L, label = paste("Spec (", round(level *
        100, 0), "% CI)", sep = ""), pos = 4, cex = 1.25)
    text(x = (x.8 - 0.05), y = N + L, label = paste("Sens (",
        round(level * 100, 0), "% CI)", sep = ""), pos = 4,
        cex = 1.25)
    text(x = (x.9 - 0.05), y = N + L, label = paste("Spec (",
        round(level * 100, 0), "% CI)", sep = ""), pos = 4,
        cex = 1.25)
    lines(x = c((x.9 - 0.25), (x.9 - 0.25)), y = c(0, y.2), lty = 2,
        col = 2)
    axis(1, at = seq(x.8, (x.9 - 0.5), 0.6), labels = seq(low_axis_se,
        up_axis_se, axis_label_se))
    axis(1, at = seq(x.9, (x.10 - 0.5), 0.6), labels = seq(low_axis_sp,
        up_axis_sp, axis_label_sp))
    if (is.null(mean)) {
        dummy = 2
    }
    else {
        mean_pos = paste(sprintf(paste("%0.", digits, "f",
            sep = ""), mean[1]), " (", sprintf(paste("%0.",
            digits, "f", sep = ""), mean[2]), "-", sprintf(paste("%0.",
            digits, "f", sep = ""), mean[3]), ")", sep = "")
        mean_neg = paste(sprintf(paste("%0.", digits, "f",
            sep = ""), mean[4]), " (", sprintf(paste("%0.",
            digits, "f", sep = ""), mean[5]), "-", sprintf(paste("%0.",
            digits, "f", sep = ""), mean[6]), ")", sep = "")
        text(x = x.1, y = 1, label = "Mean estimates", pos = 4,
            cex = 1.25)
        text(x = x.6, y = 1, label = mean_pos, pos = 4, cex = 1.25)
        text(x = x.7, y = 1, label = mean_neg, pos = 4, cex = 1.25)
        p_pos_lo_TRANS = (x.8) + ((x.9 - 0.5) - (x.8)) * (mean[2] -
            low_axis_se)/cte_se
        p_pos_hi_TRANS = (x.8) + ((x.9 - 0.5) - (x.8)) * (mean[3] -
            low_axis_se)/cte_se
        p_pos_TRANS = (x.8) + ((x.9 - 0.5) - (x.8)) * (mean[1] -
            low_axis_se)/cte_se
        lines(x = c(p_pos_lo_TRANS, p_pos_hi_TRANS), y = c(1,
            1), lwd = 2.25)
        points(x = p_pos_TRANS, y = 1, pch = 16, cex = 1.5)
        p_neg_lo_TRANS = (x.9) + ((x.10 - 0.5) - (x.9)) * (mean[5] -
            low_axis_sp)/cte_sp
        p_neg_up_TRANS = (x.9) + ((x.10 - 0.5) - (x.9)) * (mean[6] -
            low_axis_sp)/cte_sp
        p_neg_TRANS = (x.9) + ((x.10 - 0.5) - (x.9)) * (mean[4] -
            low_axis_sp)/cte_sp
        lines(x = c(p_neg_lo_TRANS, p_neg_up_TRANS), y = c(1,
            1), lwd = 2.25)
        points(x = p_neg_TRANS, y = 1, pch = 16, cex = 1.5)
    }
    if (is.null(save_plot) == FALSE)
        dev.off()
}
