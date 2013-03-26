require 'irb/completion'
require 'irb/ext/save-history'

IRB.conf[:PROMPT_MODE] = :SIMPLE

# wirble for permanent eyes bleeding
require 'wirble'

Wirble.init({
  :history_path =>"#{ENV['HOME']}/.irb_history",
  :history_size => 10000
})

Wirble.colorize

Wirble::Colorize.colors = {
  # delimiter colors
  :comma              => :blue,
  :refers             => :blue,

  # container colors (hash and array)
  :open_hash          => :blue,
  :close_hash         => :blue,
  :open_array         => :blue,
  :close_array        => :blue,

  # object colors
  :open_object        => :light_red,
  :object_class       => :white,
  :object_addr_prefix => :blue,
  :object_line_prefix => :blue,
  :close_object       => :light_red,

  # symbol colors
  :symbol             => :yellow,
  :symbol_prefix      => :yellow,

  # string colors
  :open_string        => :yellow,
  :string             => :yellow,
  :close_string       => :yellow,

  # misc colors
  :number             => :cyan,
  :keyword            => :green,
  :class              => :light_green,
  :range              => :red,
}
