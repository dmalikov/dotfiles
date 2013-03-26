require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:SAVE_HISTORY] = 10000
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb_history"

# wirble for permanent eyes bleeding
require 'wirble'
Wirble.init
Wirble.colorize
# the default colors suck, mod to use your own
colors = Wirble::Colorize.colors.merge({
   # set the comma color to blue
   :comma => :green,
   :refers => :green,
})
Wirble::Colorize.colors = colors
