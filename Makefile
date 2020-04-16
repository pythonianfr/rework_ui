FLAGS ?= --optimize

all: ui_elm

ui_elm:
	cd elm && elm make src/Main.elm $(FLAGS) --output ../rework_ui/rui_static/rework_ui_elm.js

elm-test:
	cd elm && elm-test

clean: cleanstuff cleanbuild

cleanstuff:
	cd elm && rm elm-stuff -rf

cleanbuild:
	rm rework_ui/rui_static/rework_ui_elm.js -f
