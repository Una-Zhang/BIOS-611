.PHONY: clean

clean:
	rm -f output/*

stipends_cleaned.csv: stipends.csv Project_Skeleton.R
	Rscript Project_Skeleton.R

output/Overall_Pay_by_Department.png output/Overall_Pay_across_Years.png: stipends_cleaned.csv Project_figures.R
	Rscript Project_figures.R