# doudpackage 2.1.0
* Parallel processing added as an option
* Bug correction when parsing table if only the group variable had two levels
* Removing rows with NA regarding group variable
* Remove ident on group_rows_labels
TODO
* Check for normality for each variable
* Diminish memory consumption on descTab output (does not contain initial dataset)
* Output option to display mean(SD)/median(IQR) for quantitative variable
* Add automatic group_rows with factors with more than 2 levels method: (pack_rows(label_row_css = "padding-left: 2em"))
* Add option to indent group_rows
* add option for font
* add a star/italic for significant pvalue


# doudpackage 2.0.1
* CRAN availability

## Major changes
* Added more than 2 sub-groups
* Choice between normal and non normal variables to use either t.test/Wilcox or AOV/Krusall.wallis

## Minor changes
* Faster implementation (no loop used)
