run: build
	docker run -v `pwd`:/workdir -it csg-tokyo/typelevellr:latest \
		./docker-entrypoint.sh
test: build
	docker run -t -v `pwd`:/workdir csg-tokyo/typelevellr:latest \
		stack test --allow-different-user
measure: build
	docker run -t -v `pwd`:/workdir csg-tokyo/typelevellr:latest \
		stack install && cd experiment && npm ci && \
		ruby randomchain-experiment.rb --ts -v -n 300 -m 10 \
			--num-warmup 0 --num-measure 1
build:
	docker build -t csg-tokyo/typelevellr .
.PHONY: run test build