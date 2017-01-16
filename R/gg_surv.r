

gg_surv = function(km_fit, plot_name = 'km_plot.png', include_table = T, time_unit = 'days', plot_title = '', sub_title = '', break_width = 100, x_min = 0, x_max = NULL, table_plot_width = 3200, table_plot_height = 4500) {

	library(plyr)
	library(dplyr)
	library(survival)
	library(ggplot2)
	library(scales)
	library(gridExtra)
	library(stringr)
	library(reshape2)
		
	km_data = data_frame(
		time = km_fit$time, 
		km_estimate = km_fit$surv, 
		ci_upper = km_fit$upper, 
		ci_lower = km_fit$lower,	
		n_at_risk = km_fit$n.risk, 
		n_event = km_fit$n.event, 
		n_censor = km_fit$n.censor, 
		strata = as.character(NA),
		cum_event = as.numeric(NA),
		cum_censor = as.numeric(NA)
	)	
	
	if (is.null(x_max)) {
		x_max = max(km_data$time)		
	}
	
	x_seq = seq(x_min, x_max, by = break_width)


	# for each strata, get the time that best corresponds to the x axis breaks and extract event/censor data 

	# break_width = 100 

	chart_data_list = list()

	strata_levels = km_fit$strata

	if (!is.null(strata_levels)) {

		for (it in 1:length(strata_levels)) {
			
			strata_name = names(strata_levels)[it]		

			if (it == 1) {
				strata_start = 1 		
				strata_end = strata_levels[it]
			} else {
				strata_start = strata_levels[it-1] + 1	
				strata_end = strata_levels[it-1] + strata_levels[it]
			}

			strata_index = strata_start:strata_end	 
		 	km_data$strata[strata_index] = strata_name
		 	km_data$cum_event[strata_index] = cumsum(km_data$n_event[strata_index])
		 	km_data$cum_censor[strata_index] = cumsum(km_data$n_censor[strata_index])


			times_for_cum_data = sapply(x_seq, function(x_val){					

				if (x_val == 0) {
					closest_time = min(km_data$time[strata_index])
				} else {
					vals_to_search = km_data$time[strata_index]								
					times_less_than_xval = vals_to_search[vals_to_search <= x_val]
					time_diff = abs(x_val - times_less_than_xval)
					closest_time = times_less_than_xval[time_diff == min(time_diff)]		
				}
										
				c( closest_time = closest_time , x_time = x_val)
			}) %>%
			t() %>%
			data.frame()

		 	this_cum_chart_data = filter(km_data, strata == strata_name) %>% 
		 	inner_join(times_for_cum_data, by = c('time' = 'closest_time')) %>%
			select(
				time, x_time, n_at_risk, cum_event, cum_censor, strata
				) %>%		
			melt(
				id = c('time', 'x_time', 'strata')
				) 
			
			chart_data_list[[it]] = this_cum_chart_data
		}	

		cum_chart_data = rbind.fill(chart_data_list)
		km_data$strata = with(km_data, str_replace_all(strata, pattern = '.*\\=', replacement = '') )
		cum_chart_data$variable_pretty = mapvalues(cum_chart_data$variable, c('n_at_risk', 'cum_event', 'cum_censor'), c('At Risk', 'Cumulative Events', 'Cumulative Censored')) %>% as.character()

		plot_base = ggplot(km_data, aes(time, km_estimate, colour = strata, linetype = strata))
		table_plot_base = ggplot(cum_chart_data, aes(x_time, colour = strata)) 


	} else {

		km_data$cum_event = cumsum(km_data$n_event)
		km_data$cum_censor = cumsum(km_data$n_censor)
		km_data$strata = '     '
		

		times_for_cum_data = sapply(x_seq, function(x_val){					
			if (x_val == 0) {
				closest_time = min(km_data$time)
			} else {
				vals_to_search = km_data$time								
				times_less_than_xval = vals_to_search[vals_to_search <= x_val]
				time_diff = abs(x_val - times_less_than_xval)
				closest_time = times_less_than_xval[time_diff == min(time_diff)]		
			}							
			c( closest_time = closest_time , x_time = x_val)
		}) %>%
		t() %>%
		data.frame()

		cum_chart_data = km_data %>% 
		inner_join(times_for_cum_data, by = c('time' = 'closest_time')) %>%
		select(
			time, x_time, n_at_risk, cum_event, cum_censor, strata
			) %>%		
		melt(
			id = c('time', 'x_time', 'strata')
			) 

		cum_chart_data$variable_pretty = mapvalues(cum_chart_data$variable, c('n_at_risk', 'cum_event', 'cum_censor'), c('At Risk', 'Cumulative Events', 'Cumulative Censored')) %>% as.character()

		plot_base = ggplot(km_data, aes(time, km_estimate))		
		table_plot_base = ggplot(cum_chart_data, aes(x_time)) 
	}	

	## prepare the plots ##
	km_plot = plot_base +
	geom_step(size = 1) +
	theme(
		legend.position = 'bottom'
		) +
	scale_x_continuous(breaks = x_seq) +
	scale_y_continuous(labels = percent) +
	scale_linetype(name = '') +
	scale_colour_hue(name = '') +
	labs(
		y = '\nProbability of Surviving to Time (T)',
		x = sprintf('Time (in %s)', time_unit),
		title = plot_title		
		) +
	guides(colour = guide_legend(override.aes = list(size = 2)))

	table_plot = table_plot_base + 
	facet_wrap(~variable_pretty, ncol = 1) +
	geom_text(aes(x = x_time, y = strata, label = comma(value))) +
	scale_x_continuous(limits = c(x_min, x_max)) +
	scale_colour_hue(guide = F) +
	theme(
		axis.ticks = element_blank(),
		axis.text = element_blank(),
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank()
		) +
	labs(
		y = '\n\n\n',
		x = ''
		)


	km_plot_grob = ggplotGrob(km_plot)
	table_plot_grob = ggplotGrob(table_plot)

	out_list = list(
		km_plot = km_plot, 
		table_plot = table_plot
	)

	if (include_table) {
		
		final_plot = arrangeGrob(grobs = list(km_plot_grob, table_plot_grob), heights = unit(c(5, 2.5), 'in'))

		out_list$final_plot = final_plot		
		png(plot_name, width = table_plot_width, height = table_plot_height, type = 'cairo', res = 600)
		grid.arrange(final_plot)
		dev.off()		
	}

	return(out_list)
}


## test out the function 

# library(survival)
# data(mgus2)

# km_fit = survfit(Surv(futime, event = death) ~ strata(sex), data = mgus2)
# km_fit2 = survfit(Surv(futime, event = death) ~ 1, data = mgus2)

# setwd('~')

# single_plot_results = gg_surv(km_fit2, plot_name = 'single_strata_plot.png', table_plot_height = 4500)
# strata_results = gg_surv(km_fit, plot_name = 'strata_plot.png', plot_title = 'Kaplan-Meier Plot for Time To Death\nMonoclonal gammapothy data')


