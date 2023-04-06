set history save
define hook-next
  refresh
end
#tui enable
set print thread-events off
set debuginfod enabled off
set breakpoint pending on
break __ubsan_on_report
break __asan_on_error@plt
set breakpoint pending off
