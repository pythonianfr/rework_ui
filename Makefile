FLAGS ?= --optimize

all: ui_elm

ui_elm:
	cd elm && elm make src/Main.elm $(FLAGS) --output ../rework_ui/rui_static/rework_ui_elm.js
	cd elm && elm make src/Logview.elm $(FlAGS) --output ../rework_ui/rui_static/logview.js
	cd elm && elm make src/Info.elm $(FlAGS) --output ../rework_ui/rui_static/info.js

elm-test:
	cd elm && elm-test

clean: cleanstuff cleanbuild

cleanstuff:
	cd elm && rm elm-stuff -rf

cleanbuild:
	rm rework_ui/rui_static/rework_ui_elm.js -f
	rm rework_ui/rui_static/logview.js -f
	rm rework_ui/rui_static/info.js -f
