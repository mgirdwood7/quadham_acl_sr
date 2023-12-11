## Figures for paper
## Requires all models to be run/loaded in environment

## Quad figures

## Slow Isokinetic
quadslow <- mv_plotdetails(slow_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 5000), 
             trans = custom_trans, 
             range = c(1, 7), 
             breaks = c(50, 100, 500, 1000, 5000)) +
  labs(title = "Within-Person \n") +
  theme(plot.title = element_text(size = 16))

quadcontslow <- mv_plotdetails(qcont_slow_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 5000), 
             trans = custom_trans, 
             range = c(1, 7), 
             breaks = c(50, 100, 500, 1000, 5000)) +
  labs(title = "Between-Person \n") +
  theme(plot.title = element_text(size = 16))

ggarrange(quadslow, quadcontslow, common.legend = TRUE, legend = "right")
ggsave("output/plots/quad_slow.png", width = 10, height = 5)
ggsave("output/plots/quad_slow.jpeg", width = 10, height = 5, dpi = 300)


## Fast Isokinetic
quadfast <- mv_plotdetails(fast_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 500),
             range = c(0, 7),
             breaks = c(50, 100, 250, 500)) +
  labs(title = "Within-Person \n") +
  theme(plot.title = element_text(size = 16))

quadcontfast <- mv_plotdetails(qcont_fast_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 500),
             range = c(0, 7),
             breaks = c(50, 100, 250, 500)) +
  labs(title = "Between-Person \n") +
  theme(plot.title = element_text(size = 16))

ggarrange(quadfast, quadcontfast, common.legend = TRUE, legend = "right")
ggsave("output/plots/quad_fast.png", width = 10, height = 5)
ggsave("output/plots/quad_fast.jpeg", width = 10, height = 5, dpi = 300)




## Isometric
quadiso <- mv_plotdetails(iso_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 500),
             range = c(0, 7),
             breaks = c(50, 100, 250, 500)) +
  labs(title = "Within-Person \n") +
  theme(plot.title = element_text(size = 16))

quadcontiso <- mv_plotdetails(qcont_iso_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 500),
             range = c(0, 7),
             breaks = c(50, 100, 250, 500)) +
  labs(title = "Between-Person \n") +
  theme(plot.title = element_text(size = 16))

ggarrange(quadiso, quadcontiso, common.legend = TRUE, legend = "right")
ggsave("output/plots/quad_iso.png", width = 10, height = 5)
ggsave("output/plots/quad_iso.jpeg", width = 10, height = 5, dpi = 300)





##### HS Figures

## Slow Isokinetic
hsslow <- mv_plotdetails(hs_slow_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 5000), 
             trans = custom_trans, 
             range = c(1, 7), 
             breaks = c(50, 100, 500, 1000, 5000)) +
  labs(title = "Within-Person \n") +
  theme(plot.title = element_text(size = 16))

hscontslow <- mv_plotdetails(hcont_slow_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 5000), 
             trans = custom_trans, 
             range = c(1, 7), 
             breaks = c(50, 100, 500, 1000, 5000)) +
  labs(title = "Between-Person \n") +
  theme(plot.title = element_text(size = 16))

ggarrange(hsslow, hscontslow, common.legend = TRUE, legend = "right")
ggsave("output/plots/hs_slow.png", width = 10, height = 5)
ggsave("output/plots/hs_slow.jpeg", width = 10, height = 5, dpi = 300)


## Fast Isokinetic
hsfast <- mv_plotdetails(hs_fast_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 500),
             range = c(0, 7),
             breaks = c(50, 100, 250, 500)) +
  labs(title = "Within-Person \n") +
  theme(plot.title = element_text(size = 16))

hscontfast <- mv_plotdetails(hcont_fast_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 500),
             range = c(0, 7),
             breaks = c(50, 100, 250, 500)) +
  labs(title = "Between-Person \n") +
  theme(plot.title = element_text(size = 16))

ggarrange(hsfast, hscontfast, common.legend = TRUE, legend = "right")
ggsave("output/plots/hs_fast.png", width = 10, height = 5)
ggsave("output/plots/hs_fast.jpeg", width = 10, height = 5, dpi = 300)


## Isometric
hsiso <- mv_plotdetails(hs_iso_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 200),
             range = c(0, 7),
             breaks = c(50, 100, 200)) +
  labs(title = "Within-Person \n") +
  theme(plot.title = element_text(size = 16))

hscontiso <- mv_plotdetails(hcont_iso_mv, include_pi = TRUE) + 
  scale_size(limits = c(1, 200),
             range = c(0, 7),
             breaks = c(50, 100, 200)) +
  labs(title = "Between-Person \n") +
  theme(plot.title = element_text(size = 16))

ggarrange(hsiso, hscontiso, common.legend = TRUE, legend = "right")
ggsave("output/plots/hs_iso.png", width = 10, height = 5)
ggsave("output/plots/hs_iso.jpeg", width = 10, height = 5, dpi = 300)


