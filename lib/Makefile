
SUBDIRS	= mbrain

.PHONY: all debug clean $(SUBDIRS) $(SUBDIRS:%=clean-%)
all: $(SUBDIRS)
clean: $(SUBDIRS:%=clean-%)
debug: $(SUBDIRS:%=debug-%)

$(SUBDIRS):
	cd $@ && $(MAKE)

$(SUBDIRS:%=clean-%):
	cd $(patsubst clean-%,%,$@) && $(MAKE) clean

$(SUBDIRS:%=debug-%):
	cd $(patsubst debug-%,%,$@) && $(MAKE) debug
