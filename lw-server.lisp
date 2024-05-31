(require "asdf")
(push #p"~/sly/slynk/" asdf:*central-registry*)
(asdf:load-system :slynk)
(slynk:create-server :port 4008)
