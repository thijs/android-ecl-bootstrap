#
# Recommended settings for debuggin either ecl_min or ecl.
#

macro define NIL (cl_symbols)
macro define T (cl_symbols+1)

define ecl_print
	set $cl_debug_arg = (long)($arg0)
	print $cl_debug_arg
	set $cl_debug_stream = ecl_make_string_output_stream(4096, 0)

	set $cl_debug_imm = 3 & $cl_debug_arg
	set $cl_debug_type = ($cl_debug_imm ? $cl_debug_imm : ((cl_object)$cl_debug_arg)->d.t)
	print (cl_type) $cl_debug_type

	# not safe for printing
	if $cl_debug_type > 0
	    set $cl_debug_void = si_write_ugly_object( $cl_debug_arg, $cl_debug_stream)
	    print $cl_debug_stream->stream.object0->base_string.self

	    set $cl_debug_stream->stream.object0->base_string.fillp = 0
	    set $cl_debug_void = ecl_princ( $cl_debug_arg, $cl_debug_stream)
	    # ensure string is terminated
	    set $cl_debug_stream->stream.object0->base_string.self[$cl_debug_stream->stream.object0->base_string.fillp] = 0
	    print $cl_debug_stream->stream.object0->base_string.self
	end
end


define ecl_eval
	set $cl_debug_str = ecl_make_simple_base_string( $arg0, -1)
	set $cl_debug_stream = ecl_make_string_input_stream( $cl_debug_str, 0, ((struct ecl_base_string*)$cl_debug_str)->fillp)
	set $cl_debug_form = cl_read(1, $cl_debug_stream)
	disable breakpoints
	ecl_print cl_eval($cl_debug_form)
	enable breakpoints
end

set sysroot device/
set solib-search-path 
/opt/android-64/EQL5-Android/examples/testapp/android-build/libs/arm64-v8a:/opt/android-64/EQL5-Android/examples/testapp/android-build/assets/lib
# :/home/thijs/home_root/Downloads/android-dev/oneplustwo

set auto-solib-add on
set target-async on


delete break
break cl_cos
break cl_error
break cl_cerror
break CEerror
break FEerror
break FEunbound_variable
break FEundefined_function
break FEwrong_type_argument
break FEinvalid_function

set confirm off

handle SIG33 nostop
handle SIG37 nostop
handle SIGBUS nostop
handle SIGPWR nostop pass noprint
handle SIGXCPU nostop pass noprint
handle EXC_BAD_ACCESS nostop pass noprint

target remote :5039
continue
