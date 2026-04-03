# This script produces the figures for "State or nation, sector or system? How granularity shapes U.S. energy modeling results"
# Most of the packages are available at CRAN but rgcam should be installed using devtools
## install_github("JGCRI", "rgcam")

library(rgcam)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# set colors ----
light_blue_grey <- "#C4CEDC"
light_grey <- "#ECECEC"
extra_light_grey <- "#FAFAFA"
grey <- "#9E9E9E"
dark_grey <- "#565656"
black <- "#080808"
dark_blue <- "#00449C"
light_blue <- "#62C0F5"
green <- "#169E70"
light_green <- "#6FD02A"
yellow <- "#F7EB49"
marigold <- "#FFCC1C"
orange <- "#FF8100"
pink <- "#FF96B8"
red <- "#C71300"
fuscia <- "#B92DAA"
dark_red <- "#800101"
purple <- "#7840A0"
light_orange <- "#fbb372"
light_yellow <-"#FDE698"
fuel_color_scheme <- c("Biomass" = green,
                       "Biomass CCS" = light_green,
                       "Coal" = dark_grey,
                       "Coal CCS" = black,
                       "E-fuels" = red,
                       "Electricity" = marigold,
                       "Electricity: Low Emission" = marigold,
                       "Electricity: Unabated Fossil" = marigold,
                       "Hydrogen" = light_grey,
                       "Hydrogen CT" = light_grey,
                       "Nuclear" = orange,
                       "Gas" = red,
                       "Natural Gas" = red,
                       "Gas CC" = red,
                       "Gas CT" = purple,
                       "Gas CC CCS" = dark_red,
                       "Geothermal" = pink,
                       "Hydro" = dark_blue,
                       "Liquids" = purple,
                       "Refined Liquids" = purple,
                       "Refined Liquids: Biofuels" = purple,
                       "Refined Liquids: Fossil" = purple,
                       "Liquids CCS" = fuscia,
                       "Solar" = yellow,
                       "Solar (Dist)" = marigold,
                       "Wind" = light_blue,
                       "Cogeneration" = extra_light_grey)

elec_tech_color_scheme <- c("Biomass" = "#169E70",
                            "Biomass CCS" = "#6FD02A",
                            "Coal" = "#565656",
                            "Coal CCS" = "black",
                            "Nuclear" = "#FF8100",
                            "Gas CC" = "#C71300",
                            "Gas CT" = "#7840A0",
                            "Gas CC CCS" = "#800101",
                            "Geothermal" = "#FF96B8",
                            "Hydro" = "#00449C",
                            "Liquids" = "#7840A0",
                            "Liquids CCS" = "#B92DAA",
                            "Solar" = "#F7EB49",
                            "Solar (Dist)" = "#FFCC1C",
                            "Wind" = "#62C0F5")

# read mapping files ----
elec_gen_tech_mapping <- read_csv("./mappings/elec_gen_tech_mapping.csv")
fuel_mapping <- read_csv("./mappings/fuel_mapping.csv")
enduse_technology_mapping <- read_csv("./mappings/enduse_technology_mapping.csv")

# define constants ----
ANALYSIS_YEARS <- 2021:2050
CONV_H2_EJ_MT <- 1/0.1202
CONV_elec_EJ_TWh <- 1/.0036
gdp_deflator_1975_2022<-0.2351852
gdp_deflator_2020_1975<-3.818947

# read rgcam proj file----
ief_output.proj <- loadProject("./ief_output.proj")
listQueries(ief_output.proj)

REQUERY_DATA <- FALSE
if(REQUERY_DATA){
  scenarios <- c("GCAM_RefTech",
                 "GCAM_NZ_AdvTech",
                 "GCAM_RefTech_AdvBld",
                 "GCAM_RefTech_AdvElec",
                 "GCAM_RefTech_AdvTrn",
                 "GCAM-USA_RefTech",
                 "GCAM-USA_NZ_AdvTech")

  for(i in scenarios){
    conn <- localDBConn("/Users/d3p747/Desktop/stash/decarb/output", "database_basexdb")
    ief_output.proj <- addScenario(conn, ief_output.proj, i, "BatchQueries_ief.xml", clobber = TRUE)
  }
}

# Rename scenarios for the figure naming
names(ief_output.proj) <- sub("USAreg_", "GCAM_", names(ief_output.proj))
names(ief_output.proj) <- sub("GCAMUSA_", "GCAM-USA_", names(ief_output.proj))
ief_output.proj <- lapply(ief_output.proj, function(inner_list) {
  lapply(inner_list, function(df) {
    df$scenario <- sub("USAreg_", "GCAM_", df$scenario)
    df$scenario <- sub("GCAMUSA_", "GCAM-USA_", df$scenario)
    df
  })
})

# Q1: GCAM vs GCAM-USA ----
  # a: renewable electrolysis hydrogen production (quantity and share)----
elh2 <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         sector == "H2 central production",
         subsector == "hybrid",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, year) %>%
  summarise(MtH2 = sum(value) * CONV_H2_EJ_MT)

   # a: green hydrogen share (national)
totalh2 <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         sector == c("H2 central production") | subsector == "onsite production",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, year) %>%
  summarise(MtH2 = sum(value) * CONV_H2_EJ_MT)

elh2_share <- left_join(elh2, totalh2, by = c("scenario", "year"), suffix = c(".el", ".total")) %>%
  mutate(el_share = MtH2.el / MtH2.total,
         scenario = sub("_RefTech", "", scenario))

   # a: renewable electrolysis hydrogen share by state
elh2_state <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         sector == "H2 central production",
         subsector == "hybrid",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, region, year) %>%
  summarise(MtH2 = sum(value) * CONV_H2_EJ_MT)

totalh2_state <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         sector == c("H2 central production") | subsector == "onsite production",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, region, year) %>%
  summarise(MtH2 = sum(value) * CONV_H2_EJ_MT)

elh2_share_state <- left_join(elh2_state, totalh2_state, by = c("scenario", "region", "year"), suffix = c(".el", ".total")) %>%
  mutate(el_share = MtH2.el / MtH2.total,
         scenario = sub("_RefTech", "", scenario))

elh2_GCAMUSA_2040 <- filter(elh2_share_state, year == 2040, scenario == "GCAM-USA")
elh2_share_usa_2040 <- elh2_share_state$el_share[elh2_share_state$year == 2040 & elh2_share_state$scenario == "GCAM"]
elh2_GCAMUSA_2040 %>%
  mutate(label_hjust = if_else(region == 'OK',0,0.5))->elh2_GCAMUSA_2040_label
fig1b <- ggplot(elh2_GCAMUSA_2040,
       aes(x = MtH2.total, y = el_share)) +
  geom_point(aes(color = scenario)) +
  geom_abline(slope = 0, intercept = elh2_share_usa_2040, linetype = "dashed") +
  geom_text(data = subset(elh2_GCAMUSA_2040_label, el_share > 0.0566 | MtH2.total > 0.5),
            aes(label = region,hjust=label_hjust),
            vjust = -0.5) +
  geom_text(data = subset(elh2_GCAMUSA_2040, el_share < 0.0566  & el_share > 0.0475),
            aes(label = region),
            vjust = 1.5) +
  annotate(
    "text",
    x = 0.6,
    y = 0.053,
    label = "GCAM",
    vjust = 1.5
  ) +
  ylab("Renewable Electrolysis Share") +
  xlab("2040 Hydrogen Production (Mt/yr)") +
  theme_bw() +
  scale_color_manual(values = c("GCAM-USA" = "blue", "GCAM" = "gray")) +
  theme(legend.position = "none")
ggsave("fig1b_elh2_GCAMUSA_2040.png")

elec_share_scale_multiplier <- 0.005
fig1a <- ggplot(filter(elh2_share, year >= 2030)) +
  xlab("Year") +
  geom_bar(aes(x = year, y = MtH2.total, fill = scenario, linetype = scenario), position = "dodge", stat = "identity") +
  scale_y_continuous(
    name = "Mt/yr of total hydrogen production (bars)",
    sec.axis = sec_axis(~.*elec_share_scale_multiplier, name="Renewable electrolysis share (lines)")
  ) +
  geom_line(aes(x = year, y = el_share / elec_share_scale_multiplier, linetype = scenario, color = scenario)) +
  theme_bw() +
  scale_color_manual(values = c("GCAM-USA" = "blue", "GCAM" = "gray20")) +
  scale_fill_manual(values = c("GCAM-USA" = "lightblue", "GCAM" = "gray50")) +
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.3,.85))
ggsave("fig1a_h2_prod_elshare.png")

fig1 <- fig1a + fig1b + plot_layout(widths = c(0.7, 1))
ggsave("fig1_elh2.png", height = 6, width = 10, unit = "in")

elh2_share %>%
  select(scenario, year, MtH2.el) %>%
  spread(key = scenario, value = MtH2.el) %>%
  mutate(diff = GCAM - `GCAM-USA`,
         diff_pct = diff / GCAM - 1)

  # b: green ammonia production quantity ----
green_central_h2_shares <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_NZ_AdvTech", "GCAM-USA_RefTech", "GCAM-USA_NZ_AdvTech"),
         sector == "H2 central production") %>%
  group_by(scenario, region, sector, year) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  filter(subsector == "hybrid") %>%
  select(scenario, region, year, share)

green_nh3_state <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         sector == "ammonia",
         technology == "hydrogen") %>%
  left_join(green_central_h2_shares, by = c("scenario", "region", "year")) %>%
  mutate(green_nh3 = value * share,
         scenario = sub("_RefTech", "", scenario)) %>%
  select(scenario, region, year, green_nh3)

green_nh3_tot <- green_nh3_state %>%
  group_by(scenario, year) %>%
  summarise(green_nh3 = sum(green_nh3)) %>%
  ungroup()

# x: states. y: cost of renewable electrolysis. size: scale of ammonia production in 2021.
nh3tot_2021 <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario == "GCAM-USA_RefTech",
         sector == "ammonia",
         subsector == "gas",
         year == 2021) %>%
  select(scenario, region, nh3tot = value)

renewh2_cost <- getQuery(ief_output.proj, "costs by tech") %>%
  filter(scenario == "GCAM-USA_RefTech",
         sector == "H2 central production",
         subsector == "hybrid",
         year == 2050) %>%
  mutate(renewh2cost = value / gdp_deflator_1975_2022 / CONV_H2_EJ_MT ) %>%
  select(scenario, region, year, renewh2cost) %>%
  left_join(nh3tot_2021, by = c("scenario", "region")) %>%
  replace_na(list(nh3tot = 0)) %>%
  filter(renewh2cost < 6)

renewh2_cost %>%
  mutate(label_vjust = if_else(region=='OH', -1,
                               if_else(region == 'IA', 1, 2)),
         label_hjust = if_else(region=='IA', -0.5, 0.5))->renewh2_cost_label
fig2a <- ggplot() +
  geom_point(data = renewh2_cost %>% filter(nh3tot==0),
             aes(x = nh3tot, y = renewh2cost, size = nh3tot),shape=1) +
  geom_point(data = renewh2_cost %>% filter(nh3tot!=0),
               aes(x = nh3tot, y = renewh2cost, size = nh3tot,
                   fill=region),shape=21) +
  scale_fill_manual(values = c(  CA = "orange",
                                 LA = "blue",
                                 TX = "brown",
                                 FL = "steelblue",
                                 GA = "seagreen",
                                 IA = "yellow",
                                 MO = "green",
                                 NC = "skyblue",
                                 OH = "pink"), guide = "none")+
  geom_text(data = subset(renewh2_cost_label, nh3tot > 0.1),
            aes(x = nh3tot, y = renewh2cost,label = region,
                vjust = label_vjust, hjust = label_hjust),
            show.legend = FALSE) +
  geom_abline(slope = 0,
              intercept = 2.64,
              linetype = "dashed") +
  annotate("text", x = 2.5, y = 2.75,label = "GCAM", vjust = 1.5) +
  ylab("2050 Renewable Hydrogen Producer Price ($/kg)") +
  xlab("2021 Ammonia Production (Mt/yr)") +
  theme_bw() +
  guides(size = guide_legend(title = "Ammonia\n(Mt/yr)")) +
  ylim(c(2, 4.5))
ggsave("fig2a_nh3_vs_renewh2cost.png")

# green ammonia production in 2050
fig2b <- ggplot(filter(green_nh3_state, year == 2050)) +
  geom_bar(aes(x = scenario, y = green_nh3, fill = region),
           stat = "identity", position = "stack") +
  theme_bw() +
  xlab("") +
  ylab("2050 Green Ammonia Production (Mt/yr)") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(USA = "gray",
                               CA = "orange",
                               LA = "blue",
                               TX = "brown",
                               FL = "steelblue",
                               GA = "seagreen",
                               IA = "yellow",
                               MO = "green",
                               NC = "skyblue",
                               OH = "pink"))
ggsave("fig2b_nh3_by_state.png")

fig2 <- fig2a + fig2b + plot_layout(widths = c(2, 1))
ggsave("fig2_green_nh3.png", height = 6, width = 10, unit = "in")

green_nh3_state %>%
  group_by(scenario, year) %>%
  summarise(green_nh3 = sum(green_nh3)) %>%
  ungroup() %>%
  spread(key = scenario, value = green_nh3) %>%
  mutate(diff = GCAM - `GCAM-USA`,
         diff_pct = diff / GCAM - 1)

  # c: heat pump utilization----
hp <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         grepl("heating", sector),
         grepl("heat pump", technology),
         year %in% ANALYSIS_YEARS) %>%
  mutate(configuration = sub("_RefTech", "", scenario)) %>%
  group_by(configuration, region, year) %>%
  summarise(value = sum(value)) %>%
  mutate(growth = value / value[year == 2021]) %>%
  ungroup()

hp_total <- hp %>%
  group_by(configuration, year) %>%
  summarise(value = sum(value)) %>%
  mutate(growth = value / value[year == 2021]) %>%
  ungroup()

hp_2021_share <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         grepl("heating", sector),
         year == 2021) %>%
  mutate(configuration = sub("_RefTech", "", scenario)) %>%
  group_by(configuration, region, year) %>%
  summarise(tot_heat = sum(value)) %>%
  ungroup() %>%
  left_join(hp, by = c("configuration", "region", "year")) %>%
  mutate(hp_share = value / tot_heat) %>%
  select(configuration, region, hp_share)

hp_2050_share <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM-USA_RefTech"),
         grepl("heating", sector),
         year == 2050) %>%
  mutate(configuration = sub("_RefTech", "", scenario)) %>%
  group_by(configuration, region, year) %>%
  summarise(tot_heat = sum(value)) %>%
  ungroup() %>%
  left_join(hp, by = c("configuration", "region", "year")) %>%
  mutate(hp_share = value / tot_heat) %>%
  select(configuration, region, hp_share)

hp_growth_2050 <- filter(hp, year == 2050) %>%
  left_join(hp_2021_share %>%
              rename(hp_share_2021 = hp_share), by = c("configuration", "region")) %>%
  left_join(hp_2050_share %>%
              rename(hp_share_2050 = hp_share), by = c('configuration','region'))

fig3a <- ggplot(hp_total) +
  geom_line(aes(x = year, y = growth, linetype = configuration)) +
  theme_bw() +
  xlab("Year") +
  ylab("Growth from 2021")

fig3b <- ggplot(hp_growth_2050) +
  geom_point(aes(x = hp_share_2021, y = growth,
                 color = configuration, size = hp_share_2050,
                 alpha = configuration)) +
  geom_text(data = subset(hp_growth_2050, growth > 9),
            aes(x = hp_share_2021, y = growth, label = region),
            vjust = 0.2, hjust = -0.5) +
  geom_text(data = subset(hp_growth_2050, hp_share_2021 > 0.32),
            aes(x = hp_share_2021, y = growth, label = region),
            vjust = -0.7, hjust = 0.7) +
  theme_bw() +
  xlab("Heat pump share in 2021") +
  ylab("2021-2050 growth") +
  scale_color_manual(values = c("GCAM-USA" = "blue",
                                "GCAM" = "black"))+
  scale_alpha_manual(values = c("GCAM-USA" = 0.2,
                                "GCAM" = 0.7)) +
  scale_size(range = c(1, 4)) +
  guides(size = guide_legend(title = "Heat pump share in 2050"))+
  expand_limits(y = 0)

hp_growth_2050_USA<-hp_growth_2050 %>%
  filter(region=='USA')

fig3c<- ggplot(hp_growth_2050 %>%
         filter(configuration=='GCAM-USA') %>%
         select(region,`2021`=hp_share_2021,`2050`=hp_share_2050) %>%
         gather(year,hp_share,-region), aes(hp_share, fill = year)) +
  geom_density(alpha = 0.2)+
  geom_vline(xintercept = hp_growth_2050_USA$hp_share_2021, linetype='dashed')+
  geom_vline(xintercept = hp_growth_2050_USA$hp_share_2050, linetype='dashed')+
  theme_bw() +
  scale_fill_manual(values = c(`2021` = "brown",
                                `2050` = "cyan"))+
  xlab("Heat pump share") +
  ylab("Density") +
  guides(fill = guide_legend(title = "Heat pump share\n(GCAM-USA)"))+
  annotate("text", x = hp_growth_2050_USA$hp_share_2021+0.01, y = 5,
           label = "GCAM\n  2021", angle = 0, hjust = 0, vjust = 0) +
  annotate("text", x = hp_growth_2050_USA$hp_share_2050+0.01, y = 5,
           label = "GCAM\n  2050", angle = 0, hjust = 0, vjust = 0)

fig3 <- (fig3a / fig3c) | fig3b
ggsave("fig3_heat_pump.png", height = 6, width = 10, unit = "in")

# Q2: sectoral interactions ----
  # 1: different transportation assumptions -> elec prices to buildings, tech choice in buildings; tech choice in elec
  #. a. transportation electricity demand
trnelec_ref <- getQuery(ief_output.proj, "inputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvTrn"),
         input == "elect_td_trn",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, year) %>%
  summarise(trn_elec = sum(value) * CONV_elec_EJ_TWh) %>%
  ungroup() %>%
  mutate(scenario = sub("GCAM_", "", scenario))

fig4a<- ggplot() +
  geom_point(data=trnelec_ref %>%
               filter(scenario=='RefTech_AdvTrn'),
             aes(x = year, y = trn_elec, color = scenario), shape = 16, size = 2)+
  geom_line(data=trnelec_ref,
            aes(x = year, y = trn_elec, color = scenario)) +
  xlab("Year") +
  ylab("Electricity to transportation (TWh/yr)") +
  theme_bw() +
  scale_color_manual(values = c(RefTech = "black", RefTech_AdvTrn = "orange")) +
  theme(legend.position = "none")+
  ylim(c(0,1200))

elecprice_ref <- getQuery(ief_output.proj, "prices by sector") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvTrn"),
         sector == "elect_td_bld",
         year %in% ANALYSIS_YEARS) %>%
  mutate(elecprice = value * gdp_deflator_2020_1975 / CONV_elec_EJ_TWh * 100) %>%
  select(scenario, year, elecprice) %>%
  mutate(scenario = sub("GCAM_", "", scenario))

# buildings sector electricity consumption and share of tfe
bldelec_ref <- getQuery(ief_output.proj, "inputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvTrn"),
         grepl("resid|comm", sector),
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, input, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, year) %>%
  mutate(elec_share = value / sum(value)) %>%
  ungroup() %>%
  filter(input == "elect_td_bld") %>%
  mutate(scenario = sub("GCAM_", "", scenario),
         TWh = value * CONV_elec_EJ_TWh) %>%
  select(-value)

bldelec_price_ref <- left_join(bldelec_ref, elecprice_ref, by = c("scenario", "year"))

bldelec_multiplier <- 2400/10
fig4b<- ggplot() +
  geom_point(data = bldelec_price_ref %>%
               filter(scenario == 'RefTech_AdvTrn'),
             aes(x = year, y = TWh, color = scenario), shape = 16, size = 2) +
  geom_line(data = bldelec_price_ref %>%
              filter(scenario == 'RefTech_AdvTrn'), aes(x = year, y = TWh, color = scenario)) +
  # YZ fine-tune of figures- the black line of RefTech is plotted the last, so it won't get overlapped
  geom_line(data = bldelec_price_ref %>%
              filter(scenario == 'RefTech'), aes(x = year, y = TWh, color = scenario)) +
  scale_y_continuous(
    name = "Buildings electricity demand (TWh/yr)",
    limits = c(2400, 3600),
    sec.axis = sec_axis(~./bldelec_multiplier, name="Electricity price (2020 cents/kwh)")
  ) +
  geom_point(data = bldelec_price_ref %>%
               filter(scenario=='RefTech_AdvTrn'),
            aes(x = year, y = elecprice * bldelec_multiplier, color = scenario), shape=16, size = 2) +
  geom_line(data = bldelec_price_ref %>%
              filter(scenario == 'RefTech_AdvTrn'),
            aes(x = year, y = elecprice * bldelec_multiplier, color = scenario), linetype = "solid") +
  geom_line(data = bldelec_price_ref %>%
              filter(scenario == 'RefTech'),
            aes(x = year, y = elecprice * bldelec_multiplier, color = scenario), linetype = "solid") +
  annotate(
    "text", x = 2045, y = 2650, label = "Price", angle = -8
  ) +
  annotate(
    "text", x = 2045, y = 3450, label = "Demand", angle = 15
  ) +
  xlab("Year") +
  theme_bw() +
  scale_color_manual(values = c(RefTech = "black", RefTech_AdvTrn = "orange")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

# print the differences for the text
select(bldelec_price_ref, scenario, year, TWh) %>%
  spread(key = scenario, value = TWh) %>%
  mutate(diff = RefTech_AdvTrn - RefTech,
         diffpct = diff / RefTech)

select(bldelec_price_ref, scenario, year, elecprice) %>%
  spread(key = scenario, value = elecprice) %>%
  mutate(diff = RefTech_AdvTrn - RefTech,
         diffpct = diff / RefTech)

# 4c: generation share
elecgen <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvTrn"),
         sector == "electricity" | subsector == "rooftop_pv",
         year %in% ANALYSIS_YEARS) %>%
  left_join(elec_gen_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(scenario, Reporting.technology, year) %>%
  summarise(gen = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, year) %>%
  mutate(share = gen / sum(gen)) %>%
  ungroup() %>%
  mutate(scenario = sub("GCAM_", "", scenario)) %>%
  rename(Technology = Reporting.technology)

fig4c <- ggplot(filter(elecgen, year == 2050), aes(x = scenario, y = gen, fill = Technology)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw() +
  ylab("Share of total generation") +
  xlab("") +
  scale_fill_manual(values = elec_tech_color_scheme)

# print the differences for the text
filter(elecgen, year == 2050) %>%
  select(-gen) %>%
  spread(key = scenario, value = share) %>%
  mutate(diff = RefTech_AdvTrn - RefTech)

fig4 <- (fig4a / fig4b) | fig4c + plot_layout(widths = c(2, 1.2))
ggsave("fig4_trntech_elec.png", height = 6, width = 10, unit = "in")

  # 2: influence of electric sector on tech choice of end use sectors
el_producer_price <- getQuery(ief_output.proj, "prices by sector") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvElec"),
         sector == "electricity",
         year %in% ANALYSIS_YEARS) %>%
  mutate(scenario = sub("GCAM_", "", scenario),
         elecprice = value * gdp_deflator_2020_1975 / CONV_elec_EJ_TWh * 100) %>%
  select(scenario, year, elecprice)

fig5a <- ggplot(el_producer_price) +
  geom_line(aes(x = year, y = elecprice, color = scenario)) +
  xlab("Year") +
  ylab("Electricity producer price (cents/kWh)") +
  theme_bw() +
  ylim(c(0,NA)) +
  scale_color_manual(values = c(RefTech = "black", RefTech_AdvElec = "orange")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

spread(el_producer_price, key = scenario, value = elecprice) %>%
  mutate(diff = RefTech_AdvElec - RefTech,
         diffpct = diff / RefTech)

el_cons_sector <- getQuery(ief_output.proj, "inputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvElec"),
         input %in% c("elect_td_bld", "elect_td_ind", "elect_td_trn"),
         year %in% ANALYSIS_YEARS) %>%
  mutate(scenario = sub("GCAM_", "", scenario),
         sector = if_else(input == "elect_td_bld", "Building",
                          if_else(input == "elect_td_ind", "Industry", "Transportation")),
         TWh = value * CONV_elec_EJ_TWh) %>%
  group_by(scenario, sector, year) %>%
  summarise(TWh = sum(TWh)) %>%
  ungroup()

fig5b <- ggplot(el_cons_sector) +
  geom_line(aes(x = year, y = TWh, color = scenario)) +
  xlab("Year") +
  ylab("Electricity consumption (TWh/yr)") +
  theme_bw() +
  ylim(c(0,NA)) +
  facet_wrap(~sector) +
  scale_color_manual(values = c(RefTech = "black", RefTech_AdvElec = "orange")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

fig5 <- fig5a + fig5b + plot_layout(widths = c(0.5, 1))
ggsave("fig5_elec.png", height = 6, width = 10, unit = "in")

group_by(el_cons_sector, scenario, year) %>%
  summarise(TWh = sum(TWh)) %>%
  spread(key = scenario, value = TWh) %>%
  mutate(diff = RefTech_AdvElec - RefTech,
         diffpct = diff / RefTech)

# 3: influence of buildings sector on tech choice of transportation and electricity
#. a. buildings sector electricity demand
bldelec_ref <- getQuery(ief_output.proj, "inputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvBld"),
         input == "elect_td_bld",
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, year) %>%
  summarise(bld_elec = sum(value) * CONV_elec_EJ_TWh) %>%
  ungroup() %>%
  mutate(scenario = sub("GCAM_", "", scenario))

fig6a<- ggplot() +
  geom_point(data = bldelec_ref %>%
               filter(scenario=='RefTech_AdvBld'),
             aes(x = year, y = bld_elec, color = scenario), shape = 16, size = 2) +
  geom_line(data = bldelec_ref,aes(x = year, y = bld_elec, color = scenario)) +
  xlab("Year") +
  ylab("Electricity to buildings (TWh/yr)") +
  theme_bw() +
  scale_color_manual(values = c(RefTech = "black", RefTech_AdvBld = "orange")) +
  theme(legend.position = "none") +
  ylim(c(2800, 3800))

# print the differences for the text
bldelec_ref %>%
  spread(key = scenario, value = bld_elec) %>%
  mutate(diff = RefTech_AdvBld - RefTech,
         diffpct = diff / RefTech)

elecprice_bldscen <- getQuery(ief_output.proj, "prices by sector") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvBld"),
         sector == "elect_td_trn",
         year %in% ANALYSIS_YEARS) %>%
  mutate(elecprice = value * gdp_deflator_2020_1975 / CONV_elec_EJ_TWh * 100) %>%
  select(scenario, year, elecprice) %>%
  mutate(scenario = sub("GCAM_", "", scenario))

# transportation sector electricity consumption and share of tfe
trnelec_ref <- getQuery(ief_output.proj, "inputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvBld"),
         startsWith(sector, prefix = "trn_"),
         !grepl("trn_|renewable", input),
         year %in% ANALYSIS_YEARS) %>%
  group_by(scenario, input, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, year) %>%
  mutate(elec_share = value / sum(value)) %>%
  ungroup() %>%
  filter(input == "elect_td_trn") %>%
  mutate(scenario = sub("GCAM_", "", scenario),
         TWh = value * CONV_elec_EJ_TWh) %>%
  select(-value)

trnelec_price_ref <- left_join(trnelec_ref, elecprice_bldscen, by = c("scenario", "year"))

trnelec_multiplier <- 460/15
fig6b<-ggplot() +
  geom_point(data = trnelec_price_ref %>%
               filter(scenario=='RefTech_AdvBld'),
             aes(x = year, y = TWh, color = scenario), shape = 16, size = 2)+
  geom_line(data = trnelec_price_ref %>%
              filter(scenario=='RefTech_AdvBld'),
            aes(x = year, y = TWh, color = scenario)) +
  geom_line(data = trnelec_price_ref %>%
              filter(scenario=='RefTech'),
            aes(x = year, y = TWh, color = scenario)) +
  scale_y_continuous(
    name = "Transportation electricity demand (TWh/yr)",
    sec.axis = sec_axis(~./trnelec_multiplier, name="Electricity price (2020 cents/kwh)")
  ) +
  geom_point(data = trnelec_price_ref %>% filter(scenario == 'RefTech_AdvBld'),
            aes(x = year, y = elecprice * trnelec_multiplier, color = scenario), shape = 16, size = 2) +
  geom_line(data = trnelec_price_ref %>% filter(scenario =='RefTech_AdvBld'),
            aes(x = year, y = elecprice * trnelec_multiplier, color = scenario), linetype = "solid") +
  geom_line(data = trnelec_price_ref %>% filter(scenario == 'RefTech'),
            aes(x = year, y = elecprice * trnelec_multiplier, color = scenario), linetype = "solid") +
  annotate(
    "text", x = 2023, y = 400, label = "Price", angle = -15
  ) +
  annotate(
    "text", x = 2023, y = 50, label = "Demand", angle = 0
  ) +
  xlab("Year") +
  theme_bw() +
  scale_color_manual(values = c(RefTech = "black", RefTech_AdvBld = "orange")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")

# print the differences for the text
select(trnelec_price_ref, scenario, year, TWh) %>%
  spread(key = scenario, value = TWh) %>%
  mutate(diff = RefTech_AdvBld - RefTech,
         diffpct = diff / RefTech)

select(trnelec_price_ref, scenario, year, elecprice) %>%
  spread(key = scenario, value = elecprice) %>%
  mutate(diff = RefTech_AdvBld - RefTech,
         diffpct = diff / RefTech)

# 6c: generation share
elecgen_bldscen <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(scenario %in% c("GCAM_RefTech", "GCAM_RefTech_AdvBld"),
         sector == "electricity" | subsector == "rooftop_pv",
         year %in% ANALYSIS_YEARS) %>%
  left_join(elec_gen_tech_mapping, by = c("sector", "subsector", "technology")) %>%
  group_by(scenario, Reporting.technology, year) %>%
  summarise(gen = sum(value)) %>%
  ungroup() %>%
  group_by(scenario, year) %>%
  mutate(share = gen / sum(gen)) %>%
  ungroup() %>%
  mutate(scenario = sub("GCAM_", "", scenario)) %>%
  rename(Technology = Reporting.technology)

fig6c <- ggplot(filter(elecgen_bldscen, year == 2050), aes(x = scenario, y = gen, fill = Technology)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw() +
  ylab("Share of total generation") +
  xlab("") +
  scale_fill_manual(values = elec_tech_color_scheme)

# print the differences for the text
filter(elecgen_bldscen, year == 2050) %>%
  select(-gen) %>%
  spread(key = scenario, value = share) %>%
  mutate(diff = RefTech_AdvBld - RefTech)

fig6 <- (fig6a / fig6b) | fig6c + plot_layout(widths = c(2, 1.2))
ggsave("fig6_bldtech_elec.png", height = 6, width = 10, unit = "in")


# High-level comparative figures ----

# a: Electricity generation by technology----

#GCAM-USA
elec_gen_tech_GCAMUSA <- getQuery(ief_output.proj, "outputs by nested tech") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector %in% c("base load generation",
                       "elect_td_bld",
                       "intermediate generation",
                       "peak generation",
                       "subpeak generation")) %>%
  mutate(technology = gsub("\\s*\\([^)]*\\)\\s*$",'',technology)) %>%
  rename(subsector = subsector...5) %>%
  left_join(elec_gen_tech_mapping, by = c("sector", "subsector", "technology"))

#GCAM
elec_gen_tech_gcam <- getQuery(ief_output.proj, "outputs by tech") %>%
  filter(year %in% ANALYSIS_YEARS,
         sector %in% c("electricity",
                       "elect_td_bld"),
         subsector != 'elect_td_bld',
         region == 'USA') %>%
  left_join(elec_gen_tech_mapping, by = c("sector", "subsector", "technology"))

elec_gen_tech<-elec_gen_tech_gcam %>%
  select(Units,scenario,Variable,Reporting.technology,year,value) %>%
  group_by(Units,scenario,Variable,Reporting.technology,year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  bind_rows(
    elec_gen_tech_GCAMUSA %>%
      select(Units,scenario,region,Variable,Reporting.technology,year,value) %>%
      group_by(Units, scenario, Variable, Reporting.technology, year) %>%
      summarise(value = sum(value)) %>%
      ungroup()
  ) %>%
  filter(scenario %in% c("GCAM_RefTech","GCAM-USA_RefTech",
                         "GCAM_NZ_AdvTech","GCAM-USA_NZ_AdvTech")) %>%
  mutate(scenario = factor(scenario, levels = c("GCAM_RefTech","GCAM-USA_RefTech",
                                                "GCAM_NZ_AdvTech","GCAM-USA_NZ_AdvTech")))

# plot electricity generation by reporting technology
ggplot(elec_gen_tech,
       aes(x = year, y = value, fill = Reporting.technology)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = fuel_color_scheme) +
  theme_bw() +
  labs(y = "EJ/yr") + labs(x = NULL) +
  facet_wrap(~scenario, ncol = 2)

ggsave("figS1_elec_gen_by_tech.png", height = 6, width = 8, unit = "in")

# b. Final energy consumption----
inputs <- getQuery(ief_output.proj, "inputs by tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  inner_join(enduse_technology_mapping, by = c("sector", "subsector", "technology")) %>%
  inner_join(fuel_mapping, by = "input") %>%
  group_by(scenario, region, Reporting.sector, Reporting.use, Reporting.technology, Reporting.fuel, Units, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

inputs_sf <- inputs %>%
  group_by(scenario, Reporting.sector, Reporting.fuel, Units, year) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(Variable = paste("Final Energy", Reporting.sector, Reporting.fuel, sep = "|")) %>%
  filter(scenario %in% c("GCAM_RefTech","GCAM-USA_RefTech",
                         "GCAM_NZ_AdvTech","GCAM-USA_NZ_AdvTech")) %>%
  mutate(scenario = factor(scenario, levels = c("GCAM_RefTech","GCAM-USA_RefTech",
                                                "GCAM_NZ_AdvTech","GCAM-USA_NZ_AdvTech")))

inputs_f <- inputs_sf %>%
  group_by(scenario, Reporting.fuel, Units, year) %>%
  summarise(value = sum(value)) %>%
  ungroup()

# plot final energy consumption by fuel
ggplot(inputs_f,
       aes(x = year, y = value, fill = Reporting.fuel)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = fuel_color_scheme) +
  theme_bw() +
  labs(y = "EJ/yr") + labs(x = NULL) +
  facet_wrap(~scenario, ncol = 2)

ggsave("figS2_final_energy_by_fuel.png", height = 6, width = 8, unit = "in")

# plot final energy consumption by fuel in each reporting sector
sec_lst <-unique(inputs_sf$Reporting.sector)
for (i in sec_lst){
  ggplot(inputs_sf %>% filter(Reporting.sector==i),
         aes(x = year, y = value, fill = Reporting.fuel)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = fuel_color_scheme) +
    theme_bw() +
    labs(y = "EJ/yr") + labs(x = NULL) +
    facet_wrap(~scenario, ncol = 2)

  ggsave(paste0("figS2_final_energy_by_fuel_",i,".png"), height = 6, width = 8, unit = "in")
}

# Figures on renewable electricity deployment
# Comparison of wind and solar by regional configuration
elec_gen_RE <- elec_gen_tech %>%
  filter(Reporting.technology %in% c("Solar", "Solar (Dist)", "Wind")) %>%
  spread(key = scenario, value = value)
