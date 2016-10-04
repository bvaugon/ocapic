(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../LICENSE-en.                                            *)
(*                                                                       *)
(*************************************************************************)

(* Operations on internal representations of values *)

type t

type tag =
  | Null_tag    | User_01_tag | User_02_tag | User_03_tag
  | User_04_tag | User_05_tag | User_06_tag | User_07_tag
  | User_08_tag | User_09_tag | User_0a_tag | User_0b_tag
  | User_0c_tag | User_0d_tag | User_0e_tag | User_0f_tag
  | User_10_tag | User_11_tag | User_12_tag | User_13_tag
  | User_14_tag | User_15_tag | User_16_tag | User_17_tag
  | User_18_tag | User_19_tag | User_1a_tag | User_1b_tag
  | User_1c_tag | User_1d_tag | User_1e_tag | User_1f_tag
  | User_20_tag | User_21_tag | User_22_tag | User_23_tag
  | User_24_tag | User_25_tag | User_26_tag | User_27_tag
  | User_28_tag | User_29_tag | User_2a_tag | User_2b_tag
  | User_2c_tag | User_2d_tag | User_2e_tag | User_2f_tag
  | User_30_tag | User_31_tag | User_32_tag | User_33_tag
  | User_34_tag | User_35_tag | User_36_tag | User_37_tag
  | User_38_tag | User_39_tag | User_3a_tag | User_3b_tag
  | User_3c_tag | User_3d_tag | User_3e_tag | User_3f_tag
  | User_40_tag | User_41_tag | User_42_tag | User_43_tag
  | User_44_tag | User_45_tag | User_46_tag | User_47_tag
  | User_48_tag | User_49_tag | User_4a_tag | User_4b_tag
  | User_4c_tag | User_4d_tag | User_4e_tag | User_4f_tag
  | User_50_tag | User_51_tag | User_52_tag | User_53_tag
  | User_54_tag | User_55_tag | User_56_tag | User_57_tag
  | User_58_tag | User_59_tag | User_5a_tag | User_5b_tag
  | User_5c_tag | User_5d_tag | User_5e_tag | User_5f_tag
  | User_60_tag | User_61_tag | User_62_tag | User_63_tag
  | User_64_tag | User_65_tag | User_66_tag | User_67_tag
  | User_68_tag | User_69_tag | User_6a_tag | User_6b_tag
  | User_6c_tag | User_6d_tag | User_6e_tag | User_6f_tag
  | User_70_tag | User_71_tag | User_72_tag | User_73_tag
  | User_74_tag | User_75_tag | User_76_tag | User_77_tag
  | User_78_tag | User_79_tag | User_7a_tag | User_7b_tag
  | User_7c_tag | User_7d_tag | User_7e_tag | User_7f_tag
  | User_80_tag | User_81_tag | User_82_tag | User_83_tag
  | User_84_tag | User_85_tag | User_86_tag | User_87_tag
  | User_88_tag | User_89_tag | User_8a_tag | User_8b_tag
  | User_8c_tag | User_8d_tag | User_8e_tag | User_8f_tag
  | User_90_tag | User_91_tag | User_92_tag | User_93_tag
  | User_94_tag | User_95_tag | User_96_tag | User_97_tag
  | User_98_tag | User_99_tag | User_9a_tag | User_9b_tag
  | User_9c_tag | User_9d_tag | User_9e_tag | User_9f_tag
  | User_a0_tag | User_a1_tag | User_a2_tag | User_a3_tag
  | User_a4_tag | User_a5_tag | User_a6_tag | User_a7_tag
  | User_a8_tag | User_a9_tag | User_aa_tag | User_ab_tag
  | User_ac_tag | User_ad_tag | User_ae_tag | User_af_tag
  | User_b0_tag | User_b1_tag | User_b2_tag | User_b3_tag
  | User_b4_tag | User_b5_tag | User_b6_tag | User_b7_tag
  | User_b8_tag | User_b9_tag | User_ba_tag | User_bb_tag
  | User_bc_tag | User_bd_tag | User_be_tag | User_bf_tag
  | User_c0_tag | User_c1_tag | User_c2_tag | User_c3_tag
  | User_c4_tag | User_c5_tag | User_c6_tag | User_c7_tag
  | User_c8_tag | User_c9_tag | User_ca_tag | User_cb_tag
  | User_cc_tag | User_cd_tag | User_ce_tag | User_cf_tag
  | User_d0_tag | User_d1_tag | User_d2_tag | User_d3_tag
  | User_d4_tag | User_d5_tag | User_d6_tag | User_d7_tag
  | User_d8_tag | User_d9_tag | User_da_tag | User_db_tag
  | User_dc_tag | User_dd_tag | User_de_tag | User_df_tag
  | User_e0_tag | User_e1_tag | User_e2_tag | User_e3_tag
  | User_e4_tag | User_e5_tag | User_e6_tag | User_e7_tag
  | User_e8_tag | User_e9_tag | User_ea_tag | User_eb_tag
  | User_ec_tag | User_ed_tag | User_ee_tag | User_ef_tag
  | User_f0_tag | User_f1_tag | User_f2_tag | User_f3_tag
  | User_f4_tag | User_f5_tag
  | Lazy_tag         (* 246 *)
  | Closure_tag      (* 247 *)
  | Object_tag       (* 248 *)
  | Infix_tag        (* 249 *)
  | Forward_tag      (* 250 *)
  | Abstract_tag     (* 251 *)
  | String_tag       (* 252 *)
  | Double_tag       (* 253 *)
  | Double_array_tag (* 254 *)
  | Custom_tag       (* 255 *)
  | Padding_100_tag | Padding_101_tag | Padding_102_tag | Padding_103_tag
  | Padding_104_tag | Padding_105_tag | Padding_106_tag | Padding_107_tag
  | Padding_108_tag | Padding_109_tag | Padding_10a_tag | Padding_10b_tag
  | Padding_10c_tag | Padding_10d_tag | Padding_10e_tag | Padding_10f_tag
  | Padding_110_tag | Padding_111_tag | Padding_112_tag | Padding_113_tag
  | Padding_114_tag | Padding_115_tag | Padding_116_tag | Padding_117_tag
  | Padding_118_tag | Padding_119_tag | Padding_11a_tag | Padding_11b_tag
  | Padding_11c_tag | Padding_11d_tag | Padding_11e_tag | Padding_11f_tag
  | Padding_120_tag | Padding_121_tag | Padding_122_tag | Padding_123_tag
  | Padding_124_tag | Padding_125_tag | Padding_126_tag | Padding_127_tag
  | Padding_128_tag | Padding_129_tag | Padding_12a_tag | Padding_12b_tag
  | Padding_12c_tag | Padding_12d_tag | Padding_12e_tag | Padding_12f_tag
  | Padding_130_tag | Padding_131_tag | Padding_132_tag | Padding_133_tag
  | Padding_134_tag | Padding_135_tag | Padding_136_tag | Padding_137_tag
  | Padding_138_tag | Padding_139_tag | Padding_13a_tag | Padding_13b_tag
  | Padding_13c_tag | Padding_13d_tag | Padding_13e_tag | Padding_13f_tag
  | Padding_140_tag | Padding_141_tag | Padding_142_tag | Padding_143_tag
  | Padding_144_tag | Padding_145_tag | Padding_146_tag | Padding_147_tag
  | Padding_148_tag | Padding_149_tag | Padding_14a_tag | Padding_14b_tag
  | Padding_14c_tag | Padding_14d_tag | Padding_14e_tag | Padding_14f_tag
  | Padding_150_tag | Padding_151_tag | Padding_152_tag | Padding_153_tag
  | Padding_154_tag | Padding_155_tag | Padding_156_tag | Padding_157_tag
  | Padding_158_tag | Padding_159_tag | Padding_15a_tag | Padding_15b_tag
  | Padding_15c_tag | Padding_15d_tag | Padding_15e_tag | Padding_15f_tag
  | Padding_160_tag | Padding_161_tag | Padding_162_tag | Padding_163_tag
  | Padding_164_tag | Padding_165_tag | Padding_166_tag | Padding_167_tag
  | Padding_168_tag | Padding_169_tag | Padding_16a_tag | Padding_16b_tag
  | Padding_16c_tag | Padding_16d_tag | Padding_16e_tag | Padding_16f_tag
  | Padding_170_tag | Padding_171_tag | Padding_172_tag | Padding_173_tag
  | Padding_174_tag | Padding_175_tag | Padding_176_tag | Padding_177_tag
  | Padding_178_tag | Padding_179_tag | Padding_17a_tag | Padding_17b_tag
  | Padding_17c_tag | Padding_17d_tag | Padding_17e_tag | Padding_17f_tag
  | Padding_180_tag | Padding_181_tag | Padding_182_tag | Padding_183_tag
  | Padding_184_tag | Padding_185_tag | Padding_186_tag | Padding_187_tag
  | Padding_188_tag | Padding_189_tag | Padding_18a_tag | Padding_18b_tag
  | Padding_18c_tag | Padding_18d_tag | Padding_18e_tag | Padding_18f_tag
  | Padding_190_tag | Padding_191_tag | Padding_192_tag | Padding_193_tag
  | Padding_194_tag | Padding_195_tag | Padding_196_tag | Padding_197_tag
  | Padding_198_tag | Padding_199_tag | Padding_19a_tag | Padding_19b_tag
  | Padding_19c_tag | Padding_19d_tag | Padding_19e_tag | Padding_19f_tag
  | Padding_1a0_tag | Padding_1a1_tag | Padding_1a2_tag | Padding_1a3_tag
  | Padding_1a4_tag | Padding_1a5_tag | Padding_1a6_tag | Padding_1a7_tag
  | Padding_1a8_tag | Padding_1a9_tag | Padding_1aa_tag | Padding_1ab_tag
  | Padding_1ac_tag | Padding_1ad_tag | Padding_1ae_tag | Padding_1af_tag
  | Padding_1b0_tag | Padding_1b1_tag | Padding_1b2_tag | Padding_1b3_tag
  | Padding_1b4_tag | Padding_1b5_tag | Padding_1b6_tag | Padding_1b7_tag
  | Padding_1b8_tag | Padding_1b9_tag | Padding_1ba_tag | Padding_1bb_tag
  | Padding_1bc_tag | Padding_1bd_tag | Padding_1be_tag | Padding_1bf_tag
  | Padding_1c0_tag | Padding_1c1_tag | Padding_1c2_tag | Padding_1c3_tag
  | Padding_1c4_tag | Padding_1c5_tag | Padding_1c6_tag | Padding_1c7_tag
  | Padding_1c8_tag | Padding_1c9_tag | Padding_1ca_tag | Padding_1cb_tag
  | Padding_1cc_tag | Padding_1cd_tag | Padding_1ce_tag | Padding_1cf_tag
  | Padding_1d0_tag | Padding_1d1_tag | Padding_1d2_tag | Padding_1d3_tag
  | Padding_1d4_tag | Padding_1d5_tag | Padding_1d6_tag | Padding_1d7_tag
  | Padding_1d8_tag | Padding_1d9_tag | Padding_1da_tag | Padding_1db_tag
  | Padding_1dc_tag | Padding_1dd_tag | Padding_1de_tag | Padding_1df_tag
  | Padding_1e0_tag | Padding_1e1_tag | Padding_1e2_tag | Padding_1e3_tag
  | Padding_1e4_tag | Padding_1e5_tag | Padding_1e6_tag | Padding_1e7_tag
  | Padding_1e8_tag | Padding_1e9_tag | Padding_1ea_tag | Padding_1eb_tag
  | Padding_1ec_tag | Padding_1ed_tag | Padding_1ee_tag | Padding_1ef_tag
  | Padding_1f0_tag | Padding_1f1_tag | Padding_1f2_tag | Padding_1f3_tag
  | Padding_1f4_tag | Padding_1f5_tag | Padding_1f6_tag | Padding_1f7_tag
  | Padding_1f8_tag | Padding_1f9_tag | Padding_1fa_tag | Padding_1fb_tag
  | Padding_1fc_tag | Padding_1fd_tag | Padding_1fe_tag | Padding_1ff_tag
  | Padding_200_tag | Padding_201_tag | Padding_202_tag | Padding_203_tag
  | Padding_204_tag | Padding_205_tag | Padding_206_tag | Padding_207_tag
  | Padding_208_tag | Padding_209_tag | Padding_20a_tag | Padding_20b_tag
  | Padding_20c_tag | Padding_20d_tag | Padding_20e_tag | Padding_20f_tag
  | Padding_210_tag | Padding_211_tag | Padding_212_tag | Padding_213_tag
  | Padding_214_tag | Padding_215_tag | Padding_216_tag | Padding_217_tag
  | Padding_218_tag | Padding_219_tag | Padding_21a_tag | Padding_21b_tag
  | Padding_21c_tag | Padding_21d_tag | Padding_21e_tag | Padding_21f_tag
  | Padding_220_tag | Padding_221_tag | Padding_222_tag | Padding_223_tag
  | Padding_224_tag | Padding_225_tag | Padding_226_tag | Padding_227_tag
  | Padding_228_tag | Padding_229_tag | Padding_22a_tag | Padding_22b_tag
  | Padding_22c_tag | Padding_22d_tag | Padding_22e_tag | Padding_22f_tag
  | Padding_230_tag | Padding_231_tag | Padding_232_tag | Padding_233_tag
  | Padding_234_tag | Padding_235_tag | Padding_236_tag | Padding_237_tag
  | Padding_238_tag | Padding_239_tag | Padding_23a_tag | Padding_23b_tag
  | Padding_23c_tag | Padding_23d_tag | Padding_23e_tag | Padding_23f_tag
  | Padding_240_tag | Padding_241_tag | Padding_242_tag | Padding_243_tag
  | Padding_244_tag | Padding_245_tag | Padding_246_tag | Padding_247_tag
  | Padding_248_tag | Padding_249_tag | Padding_24a_tag | Padding_24b_tag
  | Padding_24c_tag | Padding_24d_tag | Padding_24e_tag | Padding_24f_tag
  | Padding_250_tag | Padding_251_tag | Padding_252_tag | Padding_253_tag
  | Padding_254_tag | Padding_255_tag | Padding_256_tag | Padding_257_tag
  | Padding_258_tag | Padding_259_tag | Padding_25a_tag | Padding_25b_tag
  | Padding_25c_tag | Padding_25d_tag | Padding_25e_tag | Padding_25f_tag
  | Padding_260_tag | Padding_261_tag | Padding_262_tag | Padding_263_tag
  | Padding_264_tag | Padding_265_tag | Padding_266_tag | Padding_267_tag
  | Padding_268_tag | Padding_269_tag | Padding_26a_tag | Padding_26b_tag
  | Padding_26c_tag | Padding_26d_tag | Padding_26e_tag | Padding_26f_tag
  | Padding_270_tag | Padding_271_tag | Padding_272_tag | Padding_273_tag
  | Padding_274_tag | Padding_275_tag | Padding_276_tag | Padding_277_tag
  | Padding_278_tag | Padding_279_tag | Padding_27a_tag | Padding_27b_tag
  | Padding_27c_tag | Padding_27d_tag | Padding_27e_tag | Padding_27f_tag
  | Padding_280_tag | Padding_281_tag | Padding_282_tag | Padding_283_tag
  | Padding_284_tag | Padding_285_tag | Padding_286_tag | Padding_287_tag
  | Padding_288_tag | Padding_289_tag | Padding_28a_tag | Padding_28b_tag
  | Padding_28c_tag | Padding_28d_tag | Padding_28e_tag | Padding_28f_tag
  | Padding_290_tag | Padding_291_tag | Padding_292_tag | Padding_293_tag
  | Padding_294_tag | Padding_295_tag | Padding_296_tag | Padding_297_tag
  | Padding_298_tag | Padding_299_tag | Padding_29a_tag | Padding_29b_tag
  | Padding_29c_tag | Padding_29d_tag | Padding_29e_tag | Padding_29f_tag
  | Padding_2a0_tag | Padding_2a1_tag | Padding_2a2_tag | Padding_2a3_tag
  | Padding_2a4_tag | Padding_2a5_tag | Padding_2a6_tag | Padding_2a7_tag
  | Padding_2a8_tag | Padding_2a9_tag | Padding_2aa_tag | Padding_2ab_tag
  | Padding_2ac_tag | Padding_2ad_tag | Padding_2ae_tag | Padding_2af_tag
  | Padding_2b0_tag | Padding_2b1_tag | Padding_2b2_tag | Padding_2b3_tag
  | Padding_2b4_tag | Padding_2b5_tag | Padding_2b6_tag | Padding_2b7_tag
  | Padding_2b8_tag | Padding_2b9_tag | Padding_2ba_tag | Padding_2bb_tag
  | Padding_2bc_tag | Padding_2bd_tag | Padding_2be_tag | Padding_2bf_tag
  | Padding_2c0_tag | Padding_2c1_tag | Padding_2c2_tag | Padding_2c3_tag
  | Padding_2c4_tag | Padding_2c5_tag | Padding_2c6_tag | Padding_2c7_tag
  | Padding_2c8_tag | Padding_2c9_tag | Padding_2ca_tag | Padding_2cb_tag
  | Padding_2cc_tag | Padding_2cd_tag | Padding_2ce_tag | Padding_2cf_tag
  | Padding_2d0_tag | Padding_2d1_tag | Padding_2d2_tag | Padding_2d3_tag
  | Padding_2d4_tag | Padding_2d5_tag | Padding_2d6_tag | Padding_2d7_tag
  | Padding_2d8_tag | Padding_2d9_tag | Padding_2da_tag | Padding_2db_tag
  | Padding_2dc_tag | Padding_2dd_tag | Padding_2de_tag | Padding_2df_tag
  | Padding_2e0_tag | Padding_2e1_tag | Padding_2e2_tag | Padding_2e3_tag
  | Padding_2e4_tag | Padding_2e5_tag | Padding_2e6_tag | Padding_2e7_tag
  | Padding_2e8_tag | Padding_2e9_tag | Padding_2ea_tag | Padding_2eb_tag
  | Padding_2ec_tag | Padding_2ed_tag | Padding_2ee_tag | Padding_2ef_tag
  | Padding_2f0_tag | Padding_2f1_tag | Padding_2f2_tag | Padding_2f3_tag
  | Padding_2f4_tag | Padding_2f5_tag | Padding_2f6_tag | Padding_2f7_tag
  | Padding_2f8_tag | Padding_2f9_tag | Padding_2fa_tag | Padding_2fb_tag
  | Padding_2fc_tag | Padding_2fd_tag | Padding_2fe_tag | Padding_2ff_tag
  | Padding_300_tag | Padding_301_tag | Padding_302_tag | Padding_303_tag
  | Padding_304_tag | Padding_305_tag | Padding_306_tag | Padding_307_tag
  | Padding_308_tag | Padding_309_tag | Padding_30a_tag | Padding_30b_tag
  | Padding_30c_tag | Padding_30d_tag | Padding_30e_tag | Padding_30f_tag
  | Padding_310_tag | Padding_311_tag | Padding_312_tag | Padding_313_tag
  | Padding_314_tag | Padding_315_tag | Padding_316_tag | Padding_317_tag
  | Padding_318_tag | Padding_319_tag | Padding_31a_tag | Padding_31b_tag
  | Padding_31c_tag | Padding_31d_tag | Padding_31e_tag | Padding_31f_tag
  | Padding_320_tag | Padding_321_tag | Padding_322_tag | Padding_323_tag
  | Padding_324_tag | Padding_325_tag | Padding_326_tag | Padding_327_tag
  | Padding_328_tag | Padding_329_tag | Padding_32a_tag | Padding_32b_tag
  | Padding_32c_tag | Padding_32d_tag | Padding_32e_tag | Padding_32f_tag
  | Padding_330_tag | Padding_331_tag | Padding_332_tag | Padding_333_tag
  | Padding_334_tag | Padding_335_tag | Padding_336_tag | Padding_337_tag
  | Padding_338_tag | Padding_339_tag | Padding_33a_tag | Padding_33b_tag
  | Padding_33c_tag | Padding_33d_tag | Padding_33e_tag | Padding_33f_tag
  | Padding_340_tag | Padding_341_tag | Padding_342_tag | Padding_343_tag
  | Padding_344_tag | Padding_345_tag | Padding_346_tag | Padding_347_tag
  | Padding_348_tag | Padding_349_tag | Padding_34a_tag | Padding_34b_tag
  | Padding_34c_tag | Padding_34d_tag | Padding_34e_tag | Padding_34f_tag
  | Padding_350_tag | Padding_351_tag | Padding_352_tag | Padding_353_tag
  | Padding_354_tag | Padding_355_tag | Padding_356_tag | Padding_357_tag
  | Padding_358_tag | Padding_359_tag | Padding_35a_tag | Padding_35b_tag
  | Padding_35c_tag | Padding_35d_tag | Padding_35e_tag | Padding_35f_tag
  | Padding_360_tag | Padding_361_tag | Padding_362_tag | Padding_363_tag
  | Padding_364_tag | Padding_365_tag | Padding_366_tag | Padding_367_tag
  | Padding_368_tag | Padding_369_tag | Padding_36a_tag | Padding_36b_tag
  | Padding_36c_tag | Padding_36d_tag | Padding_36e_tag | Padding_36f_tag
  | Padding_370_tag | Padding_371_tag | Padding_372_tag | Padding_373_tag
  | Padding_374_tag | Padding_375_tag | Padding_376_tag | Padding_377_tag
  | Padding_378_tag | Padding_379_tag | Padding_37a_tag | Padding_37b_tag
  | Padding_37c_tag | Padding_37d_tag | Padding_37e_tag | Padding_37f_tag
  | Padding_380_tag | Padding_381_tag | Padding_382_tag | Padding_383_tag
  | Padding_384_tag | Padding_385_tag | Padding_386_tag | Padding_387_tag
  | Padding_388_tag | Padding_389_tag | Padding_38a_tag | Padding_38b_tag
  | Padding_38c_tag | Padding_38d_tag | Padding_38e_tag | Padding_38f_tag
  | Padding_390_tag | Padding_391_tag | Padding_392_tag | Padding_393_tag
  | Padding_394_tag | Padding_395_tag | Padding_396_tag | Padding_397_tag
  | Padding_398_tag | Padding_399_tag | Padding_39a_tag | Padding_39b_tag
  | Padding_39c_tag | Padding_39d_tag | Padding_39e_tag | Padding_39f_tag
  | Padding_3a0_tag | Padding_3a1_tag | Padding_3a2_tag | Padding_3a3_tag
  | Padding_3a4_tag | Padding_3a5_tag | Padding_3a6_tag | Padding_3a7_tag
  | Padding_3a8_tag | Padding_3a9_tag | Padding_3aa_tag | Padding_3ab_tag
  | Padding_3ac_tag | Padding_3ad_tag | Padding_3ae_tag | Padding_3af_tag
  | Padding_3b0_tag | Padding_3b1_tag | Padding_3b2_tag | Padding_3b3_tag
  | Padding_3b4_tag | Padding_3b5_tag | Padding_3b6_tag | Padding_3b7_tag
  | Padding_3b8_tag | Padding_3b9_tag | Padding_3ba_tag | Padding_3bb_tag
  | Padding_3bc_tag | Padding_3bd_tag | Padding_3be_tag | Padding_3bf_tag
  | Padding_3c0_tag | Padding_3c1_tag | Padding_3c2_tag | Padding_3c3_tag
  | Padding_3c4_tag | Padding_3c5_tag | Padding_3c6_tag | Padding_3c7_tag
  | Padding_3c8_tag | Padding_3c9_tag | Padding_3ca_tag | Padding_3cb_tag
  | Padding_3cc_tag | Padding_3cd_tag | Padding_3ce_tag | Padding_3cf_tag
  | Padding_3d0_tag | Padding_3d1_tag | Padding_3d2_tag | Padding_3d3_tag
  | Padding_3d4_tag | Padding_3d5_tag | Padding_3d6_tag | Padding_3d7_tag
  | Padding_3d8_tag | Padding_3d9_tag | Padding_3da_tag | Padding_3db_tag
  | Padding_3dc_tag | Padding_3dd_tag | Padding_3de_tag | Padding_3df_tag
  | Padding_3e0_tag | Padding_3e1_tag | Padding_3e2_tag | Padding_3e3_tag
  | Padding_3e4_tag | Padding_3e5_tag | Padding_3e6_tag | Padding_3e7_tag
  | Int_tag          (* 1000 *)
  | Out_of_heap_tag  (* 1001 *)
;;

external repr : 'a -> t = "%identity"
external obj : t -> 'a = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "caml_obj_is_block"
external is_int : t -> bool = "%obj_is_int"
external tag : t -> tag = "caml_obj_tag"
external set_tag : t -> tag -> unit = "caml_obj_set_tag"
external size : t -> int = "%obj_size"
external field : t -> int -> t = "%obj_field"
external set_field : t -> int -> t -> unit = "%obj_set_field"
external new_block : tag -> int -> t = "caml_obj_block"
external dup : t -> t = "caml_obj_dup"
external truncate : t -> int -> unit = "caml_obj_truncate"
