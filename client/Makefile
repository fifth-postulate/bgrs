.PHONY: clean all

SOURCE_DIR=src
ELM_CODE=$(shell find ${SOURCE_DIR} -name '*.elm')
LIB_DIR=lib
TARGETS=${LIB_DIR}/Main.min.js


all: ${TARGETS}
	@echo "Finished building projects"

${LIB_DIR}/%.min.js: ${LIB_DIR}/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $@


${LIB_DIR}/%.js: ${SOURCE_DIR}/%.elm ${ELM_CODE}
	elm make $< --optimize --output $@

clean:
	rm ${LIB_DIR}/*
