###############################################################################
# Script to take screenshots.
#

# Usage:
#
#      Start a terminal with a light background using Monaco 11pt.
#      Resize to 50 x 22
#      ruby screenshot.rb light
#
#    Or:
#
#      Start a terminal with a dark background using Monaco 11pt.
#      ruby screenshot.rb dark
#
#    Press "q" repeatedly (but not too fast) until done.
#

def usage
  puts "Usage: screenshot [light|dark]"
  exit(0)
end

if ARGV.empty?
  usage
elsif ARGV[0] == "light"
  themes = [8, 256, [8, "grayscale"], [256, "grayscale"],
            "deeper-blue", "dichromacy", "leuven", "light-blue",
            "tango", "tsdh-light"]
elsif ARGV[0] == "dark"
  themes = [8, 256, [8, "grayscale"], [256, "grayscale"],
            "adwaita", "manoj-dark", "misterioso", "tango-dark",
            "tsdh-dark", "wheatgrass", "whiteboard", "wombat"]
end

BACKGROUND_MODE = ARGV[0]

ENV["LESS"] = "-R"

WINDOW_ID = `osascript -e 'tell app "Terminal" to id of window 1'`.strip

def show_and_capture(name)
  # Note: screencapture is started in the background, the 0.5s delay
  # allows "less" to be started and settled down. Don't be too quick
  # when pressing "q".
  system("sleep 1s && screencapture -l#{WINDOW_ID} #{name}.png &")
  system("less hello.c")
end

def show_and_capture_with_e2ansi(theme)
  lessopen = ["|/Applications/Emacs24.4.app/Contents/MacOS/Emacs"]
  lessopen << "--batch"
  lessopen << "-Q"
  lessopen << "-l" << "../bin/e2ansi-cat"
  lessopen << "--background-mode" << BACKGROUND_MODE

  name = "default_#{BACKGROUND_MODE}"

  if theme.is_a?(Array)
    lessopen << "--color-class" << theme[1]
    name = theme[1] + "_" + BACKGROUND_MODE
    theme = theme[0]
  end

  if theme.is_a?(Integer)
    lessopen << "--colors" << theme
    name = name + "_" + theme.to_s
  elsif theme
    lessopen << "--theme" <<  theme
    name = theme
  end

  lessopen << "%s"
  ENV["LESSOPEN"]= lessopen.join(" ")

  show_and_capture(name)
end

ENV.delete("LESSOPEN")
show_and_capture("no_color_#{BACKGROUND_MODE}")

themes.each do |theme|
  show_and_capture_with_e2ansi(theme)
end
