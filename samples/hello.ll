%string = type { i8*, i64 }
%slice_obj = type opaque
%slice = type { %slice_obj* }
%shared_obj = type opaque
%shared = type { %shared_obj* }
%map_obj = type opaque
%map = type { %map_obj* }
%chan = type opaque
declare void @__gost_rt_init()
declare void @__gost_println_str(i8*, i64)
declare %slice_obj* @__gost_slice_new(i64, i64, i64, void (i8*)*)
declare void @__gost_slice_drop(%slice_obj*)
declare i64 @__gost_slice_len(%slice_obj*)
declare i8* @__gost_slice_data(%slice_obj*)
declare void @__gost_slice_bounds_check(%slice_obj*, i64)
declare void @__gost_slice_push(%slice_obj*, i8*)
declare i32 @__gost_slice_pop(%slice_obj*, i8*)
declare %shared_obj* @__gost_shared_new(i64, void (i8*)*, i8*)
declare void @__gost_shared_inc(%shared_obj*)
declare void @__gost_shared_dec(%shared_obj*)
declare i8* @__gost_shared_get_ptr(%shared_obj*)
declare i32 @__gost_shared_is_unique(%shared_obj*)
declare %map_obj* @__gost_map_new(i32, i64, i64)
declare i32 @__gost_map_get(%map_obj*, i8*, i8*)
declare void @__gost_map_set(%map_obj*, i8*, i8*)
declare i32 @__gost_map_del(%map_obj*, i8*)
declare i64 @__gost_map_len(%map_obj*)
declare void @__gost_map_drop(%map_obj*)
declare %chan* @__gost_chan_new(i64, i32)
declare i32 @__gost_chan_send(%chan*, i8*)
declare i32 @__gost_chan_can_send(%chan*)
declare i32 @__gost_chan_can_recv(%chan*)
declare i32 @__gost_chan_recv(%chan*, i8*)
declare i32 @__gost_chan_close(%chan*)
declare void @__gost_chan_drop(%chan*)
declare i32 @__gost_select_wait(%chan**, i32)
declare void @__gost_panic(i8*, i64)
declare i8* @__gost_alloc(i64, i64)
declare void @__gost_free(i8*, i64, i64)
declare void @__gost_spawn_thread(void (i8*)*, i8*)
declare %chan* @__gost_after_ms(i64)
declare void @__gost_go_spawn(void ()*)

define void @println(%string %arg0) {
entry:
  %t0 = alloca %string
  store %string %arg0, %string* %t0
  %t1 = load %string, %string* %t0
  %t2 = extractvalue %string %t1, 0
  %t3 = extractvalue %string %t1, 1
  call void @__gost_println_str(i8* %t2, i64 %t3)
  ret void
}

define void @main() {
entry:
  call void @__gost_rt_init()
  %t0 = getelementptr inbounds [11 x i8], [11 x i8]* @.str.main.0, i32 0, i32 0
  %t1 = insertvalue %string undef, i8* %t0, 0
  %t2 = insertvalue %string %t1, i64 10, 1
  %t3 = alloca %string
  store %string %t2, %string* %t3
  %t4 = load %string, %string* %t3
  call void @println(%string %t4)
  ret void
}

@.str.main.0 = private constant [11 x i8] c"\68\65\6C\6C\6F\20\67\6F\73\74\00"
