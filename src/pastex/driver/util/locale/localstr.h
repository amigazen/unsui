#ifndef LOCALSTR_H
#define LOCALSTR_H


/****************************************************************************/


/* This file was created automatically by CatComp.
 * Do NOT edit by hand!
 */


#ifndef EXEC_TYPES_H
#include <exec/types.h>
#endif

#ifdef CATCOMP_ARRAY
#undef CATCOMP_NUMBERS
#undef CATCOMP_STRINGS
#define CATCOMP_NUMBERS
#define CATCOMP_STRINGS
#endif

#ifdef CATCOMP_BLOCK
#undef CATCOMP_STRINGS
#define CATCOMP_STRINGS
#endif


/****************************************************************************/


#ifdef CATCOMP_NUMBERS

#define MSG_CATALOG_VERSION 1
#define MSG_WRONG_CATALOG_VERSION 2
#define MSG_PROJECT_MENU 100
#define MSG_OUTFIT_MENU 101
#define MSG_MOVE_MENU 102
#define MSG_RESOLUTION_MENU 103
#define MSG_PROJECT_ABOUT 110
#define MSG_PROJECT_OPENAGAIN 111
#define MSG_PROJECT_OPENNEW 112
#define MSG_PROJECT_AUTOLOADAGAIN 113
#define MSG_PROJECT_SAVEIFF 114
#define MSG_PROJECT_SHELLCOMANDS 115
#define MSG_PROJECT_PRINTPAGE 116
#define MSG_PROJECT_WBTOFRONT 117
#define MSG_PROJECT_HIDE 118
#define MSG_PROJECT_SAVECONFIG 119
#define MSG_PROJECT_QUIT 120
#define MSG_PROJECT_SHELLCOMANDS_NEWCLI 130
#define MSG_PROJECT_SHELLCOMANDS_EXECUTECOMMAND 131
#define MSG_PROJECT_SHELLCOMANDS_TEXSCRIPT 132
#define MSG_PROJECT_SHELLCOMANDS_AREXXTEXSHELL 133
#define MSG_PROJECT_SHELLCOMANDS_SETENVTEXFORMAT 134
#define MSG_PROJECT_SHELLCOMANDS_SPECIALHOST 135
#define MSG_OUTFIT_COPY 140
#define MSG_OUTFIT_LACE 141
#define MSG_OUTFIT_SCROLLBAR 142
#define MSG_OUTFIT_FULLPAGE 143
#define MSG_OUTFIT_MEASUREWINDOW 144
#define MSG_OUTFIT_BORDERLINE 145
#define MSG_OUTFIT_SETMARGIN 146
#define MSG_OUTFIT_4COLORSCREEN 147
#define MSG_OUTFIT_PAGESCROLLBAR 148
#define MSG_OUTFIT_UNIT 149
#define MSG_OUTFIT_COLOR 150
#define MSG_OUTFIT_CLONEWBCOLOR 151
#define MSG_OUTFIT_SCREENPREFS 152
#define MSG_OUTFIT_UNIT_INCH 160
#define MSG_OUTFIT_UNIT_CM 161
#define MSG_OUTFIT_UNIT_PT 162
#define MSG_MOVE_SEARCH 170
#define MSG_MOVE_PREVPAGE 171
#define MSG_MOVE_NEXTPAGE 172
#define MSG_MOVE_FIRSTPAGE 173
#define MSG_MOVE_LASTPAGE 174
#define MSG_MOVE_PAGECOUNTER 175
#define MSG_MOVE_JUMPTOPAGENUMBER 176
#define MSG_MOVE_CLEARPAGECOUNTER 177
#define MSG_MOVE_USEPHY 178
#define MSG_MOVE_USEORDERDVI 179
#define MSG_MOVE_USEPHYPREVPAGE 180
#define MSG_MOVE_USEPHYNEXTPAGE 181
#define MSG_MOVE_USEPHYFIRSTPAGE 182
#define MSG_MOVE_USEPHYLASTPAGE 183
#define MSG_NO_LIBRARY 500
#define MSG_NO_CONSOLE_DEVICE 501
#define MSG_NO_NEW_OBJECT 502
#define MSG_CANT_CLOSE_SCREEN 503
#define MSG_CLOSE_WINDOWS 504
#define MSG_SCRERR_NOERR 505
#define MSG_SCRERR_NOMONITOR 506
#define MSG_SCRERR_NOCHIPS 507
#define MSG_SCRERR_NOMEM 508
#define MSG_SCRERR_NOCHIPMEM 509
#define MSG_SCRERR_PUBNOTUNIQUE 510
#define MSG_SCRERR_UNKNOWNMODE 511
#define MSG_SCRERR_DEFAULT 512
#define MSG_CANT_OPEN_SCR_NAME 513
#define MSG_CANT_FIND_SCR_USE_WB 514
#define MSG_CANT_LOCK_PBSCR 515
#define MSG_CANT_GET_VI 516
#define MSG_CANT_GET_DI 517
#define MSG_CANT_OPEN_WIN 518
#define MSG_SET_MARGIN 519
#define MSG_SEARCH_STRING_NOT_FOUND 520
#define MSG_SEARCH_STRING_CANCELED 521
#define MSG_SHOWDVI_MESSAGE 522
#define MSG_ABORT_PRINT_PAGE 523
#define MSG_OK_CANCEL_REQSTRING 524
#define MSG_ABORT_PRINT 525
#define MSG_PRINT_IS_ABORTED 526
#define MSG_PRINT_CUR_PAGE 527
#define MSG_WIN_HEADER_FILE 528
#define MSG_WIN_HEADER_NO_FILE 529
#define MSG_UNAVAILABLE_MODE 530
#define MSG_UNKNOWN_MODE_NAME 531
#define MSG_INTERNAL_ERROR 532
#define MSG_NO_CHIPMEM 533
#define MSG_BUILD_FULL_PAGE 534
#define MSG_NO_MEM 535
#define MSG_CANT_SAVE_TO_CLIP 536
#define MSG_CANT_SAVE_TO_IFF 537
#define MSG_NO_SIGNAL 538
#define MSG_CANT_NOTIFY 539
#define MSG_MARGIN_SET 540
#define MSG_SHOWDVI_ALREADY_ACTIVE 541
#define MSG_CANT_ACCESS_FILE 542
#define MSG_AREXX_COMM_START_FAILED 543
#define MSG_AREXX_COMM_FAILED 544
#define MSG_DIV_ZERO 545
#define MSG_CANT_SET_VARIABLE 546
#define MSG_ERROR_IN_CONFIG_FILE 547
#define MSG_BETA_VERSION 548
#define MSG_INTERNAL_ERROR_W_ARG 549
#define MSG_WRONG_MAP_WIDTH 550
#define MSG_NO_DVI_FILE 551
#define MSG_DVI_FILE_LOST 552
#define MSG_UNKNOWN_POP_SUBMENU 553
#define MSG_FILENAME 554
#define MSG_ENTER_COMMAND 555
#define MSG_EXECUTE_COMMAND 556
#define MSG_CANT_EXECUTE_COMMAND 557
#define MSG_USE_WHICH_PUBSCR 558
#define MSG_DONT_FIND_PUBSCR_USE_WB 559
#define MSG_SHOWDVI_TEX_SHELL 560
#define MSG_CANT_START 561
#define MSG_START_SPECIALHOST 562
#define MSG_UNKNOWN_SUBITEM 563
#define MSG_UNKNOWN_SUBMENU 564
#define MSG_UNKNOWN_MENU 565
#define MSG_CANT_OPEN_HELP_FILE 566
#define MSG_SHOWDVI_MENU_HELP 567
#define MSG_NO_HELP_FOR_MENU 568
#define MSG_INTERNAL_ERROR_MENU_ENTRY 569
#define MSG_CONFUSED_ABOUT_REQ 570
#define MSG_SHOWDVI_ABOUT_WINDOW 571
#define MSG_CANT_OPEN_ABOUT_REQ 572
#define MSG_NEED_REQ_LIB 573
#define MSG_SHOWDVI_REQUEST 574
#define MSG_OK 575
#define MSG_CANCEL 576
#define MSG_SHOWDVI_FATAL_MESSAGE 577
#define MSG_FATAL_WRONG 578
#define MSG_FATAL_CHIPMEM 579
#define MSG_FATAL_MEMORY 580
#define MSG_FATAL_INTERNAL 581
#define MSG_FATAL_FATAL 582
#define MSG_REALLY_QUIT 583
#define MSG_YES_NO 584
#define MSG_FILE_ISNT_DVI_FILE 585
#define MSG_CANT_ALLOC_FREQ 586
#define MSG_LOAD_NEW_DVI 587
#define MSG_CANT_FOUND_FILE 588
#define MSG_CANT_ALLOC_ASLREQ 589
#define MSG_FORMAT_FILE_NAME 590
#define MSG_NOT_WHILE_PRINTING 591
#define MSG_NO_APP_ICON 592
#define MSG_ENTER_A_STRING 593
#define MSG_ENTER_A_NUMBER 594
#define MSG_CANT_OPEN_CONFIG 595
#define MSG_LOAD_CONFIG 596
#define MSG_WRITE_CONFIG 597
#define MSG_NO_AREXX_FOR_FX 598
#define MSG_NO_ON_OFF 599
#define MSG_WRONG_ON_OFF 600
#define MSG_UNKNOWN_KEYWORD 601
#define MSG_UNKNOWN_MODEID 602
#define MSG_ILLEGAL_COLOR 603
#define MSG_WRONG_COLOR_PARAMS 604
#define MSG_WRONG_DEF_RESO 605
#define MSG_WRONG_VAL_IN_RES_MENU 606
#define MSG_WRONG_UNITS 607
#define MSG_WRONG_APPICON_POS 608
#define MSG_WRONG_MIN_SCREEN_SIZE 609
#define MSG_WRONG_SCREEN_SIZE 610
#define MSG_WRONG_MONITOR_SIZE 611
#define MSG_WRONG_WIN_POS 612
#define MSG_WRONG_WIN_SIZE 613
#define MSG_WRONG_MAX_DVIBUF_SIZE 614
#define MSG_CONFIG_HEADER 615
#define MSG_CONFIG_RGB_BACK 616
#define MSG_CONFIG_RGB_FORE 617
#define MSG_CONFIG_RGB_2 618
#define MSG_CONFIG_RGB_3 619
#define MSG_CONFIG_SCROLL_STATE 620
#define MSG_CONFIG_BORDER_LINE 621
#define MSG_CONFIG_LACE 622
#define MSG_CONFIG_BEEP 623
#define MSG_CONFIG_ESC 624
#define MSG_CONFIG_QUICK_EXIT 625
#define MSG_CONFIG_POPUP 626
#define MSG_CONFIG_INT_POPUP 627
#define MSG_CONFIG_BIG_POPUP 628
#define MSG_CONFIG_MIDDLE_POPUP 629
#define MSG_CONFIG_USEPHY 630
#define MSG_CONFIG_ACT_LOAD_AGAIN 631
#define MSG_CONFIG_USE_OWN_SCR 632
#define MSG_CONFIG_SHOW_PUBSCR_NAME 633
#define MSG_CONFIG_DEF_PUBSCR_NAME 634
#define MSG_CONFIG_WIN_POS 635
#define MSG_CONFIG_WIN_SIZE 636
#define MSG_CONFIG_SCR_POS 637
#define MSG_CONFIG_SHOW_WIN_SIZE 638
#define MSG_CONFIG_4_COL_SCR 639
#define MSG_CONFIG_BACK_HOOK 640
#define MSG_CONFIG_USE_WB_COLS 641
#define MSG_CONFIG_UNIT 642
#define MSG_CONFIG_APP_ICON 643
#define MSG_CONFIG_INFO_APP_ICON 644
#define MSG_CONFIG_APP_ICON_POS 645
#define MSG_CONIFG_SCRIPT_FILE 646
#define MSG_CONFIG_TEX_SERVER 647
#define MSG_CONFIG_SCR_SIZE 648
#define MSG_CONFIG_SCR_MODE 649
#define MSG_CONFIG_MONITOR_SIZE 650
#define MSG_CONFIG_ALWBMFAST 651
#define MSG_CONFIG_SMARTWIN 652
#define MSG_CONFIG_MAX_DVIBUF 653
#define MSG_CONFIG_DEF_RESO 654
#define MSG_CONFIG_RESO_MENU 655
#define MSG_CONFIG_SAVED_OK 656
#define MSG_CANT_SAVE_CONFIG 657
#define MSG_CONFIG_APEN 658
#define MSG_CONFIG_BPEN 659
#define MSG_WRONG_PEN 660
#define MSG_MEASURE_WINDOW 661
#define MSG_CANT_OPEN_MESSWIN 662
#define MSG_MESSWIN_WIDTH 663
#define MSG_MESSWIN_HEIGHT 664
#define MSG_MESSWIN_DEL_X 665
#define MSG_MESSWIN_X 666
#define MSG_MESSWIN_DEL_Y 667
#define MSG_MESSWIN_Y 668
#define MSG_MESSWIN_MAG 669
#define MSG_AREXX_REPLY_CODE 670
#define MSG_EXECUTE_TEX_SCRIPT 671
#define MSG_NEWCLI_TITLE 672
#define MSG_PREFWIN_GADARR_0 673
#define MSG_PREFWIN_GADARR_1 674
#define MSG_PREFWIN_GADARR_2 675
#define MSG_PREFWIN_GADARR_3 676
#define MSG_PREFWIN_GADARR_4 677
#define MSG_PREFWIN_GADARR_5 678
#define MSG_PREFWIN_GADARR_6 679
#define MSG_PREFWIN_GADARR_7 680
#define MSG_PREFWIN_GADARR_8 681
#define MSG_PREFWIN_GADARR_9 682
#define MSG_PREFWIN_GADARR_10 683
#define MSG_PREFWIN_GADTOOLS 684
#define MSG_PREFWIN_WIN_TITLE 685
#define MSG_PREFWIN_CANT_OPEN 686
#define MSG_SEARCHWIN_WIN_TITLE 687
#define MSG_SEARCHWIN_DOSEARCH 688
#define MSG_SEARCHWIN_FOUND 689
#define MSG_SEARCHWIN_CANT_OPEN 690
#define MSG_SEARCHWIN_STRING 691
#define MSG_SEARCHWIN_SEARCH 692
#define MSG_SEARCHWIN_CANCEL 693
#define MSG_NO_FILE_GIVEN 694
#define MSG_BUILD_BITMAP 695
#define MSG_STACK_OVER 696
#define MSG_STACK_UNDER 697
#define MSG_PRE_IN_FILE 698
#define MSG_POST_IN_FILE 699
#define MSG_POST_POST 700
#define MSG_DVI_FILE_ERROR 701
#define MSG_ALREADY_FIRST 702
#define MSG_ALREADY_LAST 703
#define MSG_PAGE_NOT_FOUND 704
#define MSG_MISSING_PRE 705
#define MSG_WRONG_DVI_TYPE 706
#define MSG_LOADING_DVI 707
#define MSG_INCOMPLETE_DVI_PRELOAD 708
#define MSG_SCAN_DVI_FILE 709
#define MSG_HELP_HELP 710
#define MSG_HELP_FONTDIR 711
#define MSG_HELP_FONTMEM 712
#define MSG_HELP_FROM 713
#define MSG_HELP_HOFFSET 714
#define MSG_HELP_HOFFSET_2 715
#define MSG_HELP_VOFFSET 716
#define MSG_HELP_VOFFSET_2 717
#define MSG_HELP_PRELOAD 718
#define MSG_HELP_RESOLUTION 719
#define MSG_HELP_STATISTIC 720
#define MSG_HELP_DEBUGSTAT 721
#define MSG_HELP_LOGNAME 722
#define MSG_HELP_NOLOG 723
#define MSG_HELP_PRIORITY 724
#define MSG_HELP_PRINTAUTHOR 725
#define MSG_HELP_DVIFILE 726
#define MSG_COPYRIGHT 727
#define MSG_USAGE 728
#define MSG_CANT_FIND_FILE 729
#define MSG_NOT_ENOUGH_MEM_BYTES 730
#define MSG_PROGRAM_END_ERR 731
#define MSG_USER_ABORT 732
#define MSG_PROGRAM_END_OK 733
#define MSG_DVIP_PRINT_FINISHED 734
#define MSG_FATAL 735
#define MSG_LOG_FILE_CREATED 736
#define MSG_LINE_BUFFER_OVERFLOW 737
#define MSG_BREAK 738
#define MSG_BREAK_IO 739
#define MSG_CALL_MF 740
#define MSG_CANT_OPEN 741
#define MSG_DVIP_WHOLE_BITMAP_IN_RAM 742
#define MSG_CANT_ALLOC_BITMAP 743
#define MSG_DVIP_PARTS 744
#define MSG_DVIP_SAVE_CLIP 745
#define MSG_DVIP_SAVE_IFF 746
#define MSG_DVIP_CANT_SAVE_IFF 747
#define MSG_SPECIAL_TOO_LONG 748
#define MSG_NO_MEM_FOR_SPECIAL 749
#define MSG_PICT_OUT_LEFT 750
#define MSG_PICT_OUT_RIGHT 751
#define MSG_NO_MEM_FOR_SPECIAL_BITMAP 752
#define MSG_UNKNOWN_PICT_LOC 753
#define MSG_ILLEG_PS_COMM 754
#define MSG_TOO_MANY_POINTS 755
#define MSG_MALFORMED_PATH_COMM 756
#define MSG_BAD_DVI_FILE_END 757
#define MSG_BAD_FONT_DEFS 758
#define MSG_MISS_POST_POST 759
#define MSG_MISS_POST 760
#define MSG_LOG_PAGE_DIM 761
#define MSG_LOG_HORIZ 762
#define MSG_LOG_VERT 763
#define MSG_LOG_MAG 764
#define MSG_LOG_OFFSET 765
#define MSG_LOG_THOFF 766
#define MSG_LOG_HOFF 767
#define MSG_LOG_TVOFF 768
#define MSG_LOG_VOFF 769
#define MSG_LOG_RESO 770
#define MSG_DVI_STACK_TOO_SMALL 771
#define MSG_LOG_NUM_PAGES 772
#define MSG_CANT_OPEN_FONTLIB 773
#define MSG_NOT_A_FONTLIB 774
#define MSG_OLD_FONTLIB 775
#define MSG_TOO_MANY_FONTLIB_LEVELS 776
#define MSG_ERROR_READING_FONTLIB_DIR 777
#define MSG_BAD_PK_FILE 778
#define MSG_UNEXPECTED_EOF_IN_PK 779
#define MSG_EXPECTED_PRE_IN_PK 780
#define MSG_WRONG_ID_IN_PK 781
#define MSG_CHECKSUM_MISS_IN_PK 782
#define MSG_CHAR_CODE_OUT_OF_RANGE_IN_PK 783
#define MSG_PACKET_LENGTH_SMALL_IN_PK 784
#define MSG_NO_MEM_FOR_CHAR 785
#define MSG_UNEXPECTED_COMM_IN_PK 786
#define MSG_RELEASE_CHARS 787
#define MSG_INTERNAL_ERROR_SPRINTF 788
#define MSG_UNKNOWN_FORMAT_SPRINTF 789
#define MSG_STRING_OVERFLOW_SPRINTF 790
#define MSG_ERROR_SEARCH_LIST_LIB 791
#define MSG_ERROR_SEARCH_LIST_PK 792
#define MSG_FONT_UNDEFINED 793
#define MSG_FONT_ALREADY_DEFINED 794
#define MSG_FONT_STR_UNDEFINED 795
#define MSG_RELOAD_FONT 796
#define MSG_FONTMEM_USED 797
#define MSG_LOAD_LOADED_FONT 798
#define MSG_LOAD_FONT 799
#define MSG_INTERNAL_ERROR_LIB_MISMATCH 800
#define MSG_LOAD_FONT_MEM_USED 801
#define MSG_FOUND_IN_LIB 802
#define MSG_FOUND_AS_PK 803
#define MSG_MEMORY_ERROR 804
#define MSG_SUBSTITUTE_FONT 805
#define MSG_FONT_NOT_FOUND 806
#define MSG_FONT_XY_NOT_FOUND 807
#define MSG_TRY_SUBSTITUTE 808
#define MSG_PREDEF_FONT_IN_LIB 809
#define MSG_PREDEF_FONT_NOT_FOUND 810
#define MSG_INTERNAL_ERROR_FMT_STR 811
#define MSG_BYTES_USED_FOR_CHARS 812
#define MSG_SEARCHPATH_LIB_ENTRIES_NR 813
#define MSG_SEARCHPATH_PK_ENTRIES_NR 814
#define MSG_LOG_FONTDEF_START 815
#define MSG_LOG_FONTDEF_LIB_FOUND 816
#define MSG_LOG_FONTDEF_LIB_DEF 817
#define MSG_LOG_FONTDEF_LIB_NOT_FOUND 818
#define MSG_LOG_FONTDEF_PK_FOUND 819
#define MSG_LOG_FONTDEF_PK_PREDEF 820
#define MSG_LOG_FONTDEF_PK_PREDEF_NOT_FOUND 821
#define MSG_LOG_FONTDEF_NR_BASE_PK_DIRS_PREDEF 822
#define MSG_LOG_FONTDEF_NO_PK_DIRS_PREDEF 823
#define MSG_LOG_FONTDEF_END 824
#define MSG_CANT_OPEN_FONT_CONFIG 825
#define MSG_LOAD_FONT_CONFIG 826
#define MSG_CANT_PARSE_FONT_CONFIG_LINE 827
#define MSG_TOO_FEW_ARGS 828
#define MSG_NOT_A_VALID_INT 829
#define MSG_CANT_READ_DPI_FONT_COMM 830
#define MSG_UNKNOWN_KEYWORD_IN_CONFIG 831
#define MSG_FONT_REMOVED 832
#define MSG_FONT_REMOVED_USED 833
#define MSG_ERROR_IN_FONTVOLS 834
#define MSG_FONTVOLS_FIRST_DOT 835
#define MSG_COPY_FONT_TO_CACHE 836
#define MSG_CANT_COPY_FONT_TO_CACHE 837
#define MSG_PARSE_BAD_NR_ARGS 838
#define MSG_PARSE_BAD_ON_OFF 839
#define MSG_PARSE_CANT_PARSE 840
#define MSG_PARSE_CANT_PARSE_IGNORE 841
#define MSG_PARSE_ILLEG_ARG 842
#define MSG_UNPACK_RECURSIV 843
#define MSG_UNPACK_MORE_BITS 844
#define MSG_PORT_ALREADY_EXISTS 845
#define MSG_CANT_OPEN_PRINTER_PORT 846
#define MSG_CREATEEXTIO_FAILED 847
#define MSG_YOU_PRINT_GENERIC 848
#define MSG_GENERIC_PRINT_NAME 849
#define MSG_GENERIC_PRINT_RESO 850
#define MSG_GENERIC_PRINT_MAXD 851
#define MSG_PRT_ERROR_NO_ERROR 852
#define MSG_PRT_ERROR_ABORT 853
#define MSG_PRT_ERROR_NO_GFX 854
#define MSG_PRT_ERROR_ILLDIM 855
#define MSG_PRT_ERROR_NO_MEM_VARS 856
#define MSG_PRT_ERROR_NO_MEM_BUFF 857
#define MSG_PRT_ERROR_UNKNOWN_ERR 858
#define MSG_PRT_ERR 859
#define MSG_TRY_CLEAR_PRT 860
#define MSG_CLEAR_FAILED 861
#define MSG_GETOPT_LINEBUF_OVERFLOW 862
#define MSG_GETOPT_ERROR 863
#define MSG_GETOPT_ERROR_KEY 864
#define MSG_GETOPT_NO_OPTION_STRING 865
#define MSG_GETOPT_MISSING_NUM 866
#define MSG_GETOPT_NO_NUMBER 867
#define MSG_GETOPT_MISSING_REAL 868
#define MSG_GETOPT_NO_REAL 869
#define MSG_GETOPT_MISSING_TEX 870
#define MSG_GETOPT_NO_TEX 871
#define MSG_GETOPT_UNKNOWN_PARAM 872
#define MSG_GETOPT_WRONG_ONOFF_PARAM 873
#define MSG_GETOPT_NO_PARAM_EXPECTED 874
#define MSG_GETOPT_UNKNOWN_KEYWORD 875
#define MSG_GETOPT_TOO_MANY_ENV_ARGS 876
#define MSG_GETOPT_NO_MEM_FOR_ENV 877
#define MSG_GETOPT_SUPER_FILE 878
#define MSG_GETOPT_NO_INFO 879
#define MSG_GETOPT_PARAM_REQU 880
#define MSG_GETOPT_TAB_TAB_DEF 881
#define MSG_GETOPT_TAB_DEF 882
#define MSG_GETOPT_NO_DEFAULT 883
#define MSG_GETOPT_PRESS_RET 884
#define MSG_SPECIAL_WAIT_FOR_PICT 885
#define MSG_SPECIAL_EXPT_REPLY_SPECIAL 886
#define MSG_SPECIAL_RET_FROM_SPECIAL 887
#define MSG_SPECIAL_EXPT_REPLY_TPIC 888
#define MSG_SPECIAL_EXPT_REPLY_BITMAP 889
#define MSG_SPECIAL_PICT_RECEIVED 890
#define MSG_SPECIAL_FOUND_NO_SPECIAL 891
#define MSG_SPECIAL_CANT_CREATE_PORT 892
#define MSG_NO_DVI_FILENAME 893
#define MSG_CANT_OPEN_DVI_FILE 894
#define MSG_INCOMPLETE_DVI_REVERSE 895
#define MSG_INCOMPLETE_DVI 896
#define MSG_UNDEFINED_DVI_COMMAND 897
#define MSG_NO_LAND_WITH_GENERIC 898
#define MSG_NO_LAND_XDPI_YDPI 899
#define MSG_PRINTER_NAME 900
#define MSG_PRINTER_ID 901
#define MSG_MISSING_KEYWORD 902
#define MSG_INCOMPLETE_PRT_DESC 903
#define MSG_USE_DRAFT_OPT 904
#define MSG_PRT_RESO 905
#define MSG_PRT_WIDTH 906
#define MSG_PRT_PASSES 907
#define MSG_PRT_BITMAP_HEIGHT 908
#define MSG_PRT_DEF_BUF_SIZE 909
#define MSG_PRT_MOVE_TO_POINT 910
#define MSG_PRT_ONE_GRAPHIC_COMMAND 911
#define MSG_PRT_SKIP_WITH_SPACES 912
#define MSG_PRT_BAD_RESO 913
#define MSG_PRT_USE_RES 914
#define MSG_UNKNOWN_BLANKING 915
#define MSG_ILLEG_PARM_FOR_KEY 916
#define MSG_GROUPING_RANGE 917
#define MSG_CANT_OPEN_PRT_CONFIG 918
#define MSG_READ_PRT_CONFIG 919
#define MSG_THATS_ALL 920
#define MSG_AVAILABLE_PRINTERS 921
#define MSG_MAX_GROUPING 922
#define MSG_PRINT_WITH 923
#define MSG_UNKNOWN_PRT_CONFIG 924
#define MSG_PRT_NO_OUTPUT_REDIRECTION 925
#define MSG_CANT_OPEN_OUTPUT_FILE 926
#define MSG_NO_PRT_TYPE_FLAG 927
#define MSG_KEY_TWICE 928
#define MSG_KEY_CONTEXT 929
#define MSG_CANT_WRITE_OUTPUT_FILE 930
#define MSG_WRONG_NUM_COPIES 931
#define MSG_PRINT_AT_LEAST_ONE_PAGE 932
#define MSG_WRONG_RESO_FORMAT 933
#define MSG_NO_ODD_NO_EVEN 934
#define MSG_DENSITY_RANGE 935
#define MSG_BITMAP_MEM_MINIMUM 936
#define MSG_TRY_HELP_SHOWP 937
#define MSG_UNKNOWN_PAPER_SIZE 938
#define MSG_REVERSE_TWOUP_ERR 939
#define MSG_PAPER_WIDTH_HEIGHT_ERR 940
#define MSG_OPTIONS_HELP 941
#define MSG_OPTIONS_GUI 942
#define MSG_OPTIONS_FONTDIR 943
#define MSG_OPTIONS_FONTMEM 944
#define MSG_OPTIONS_MAXBITMEM 945
#define MSG_OPTIONS_MAXBITMEM2 946
#define MSG_OPTIONS_PRTBUFFER 947
#define MSG_OPTIONS_FROM 948
#define MSG_OPTIONS_TO 949
#define MSG_OPTIONS_NUMBER 950
#define MSG_OPTIONS_ODD 951
#define MSG_OPTIONS_EVEN 952
#define MSG_OPTIONS_PHYSICAL 953
#define MSG_OPTIONS_HOFFSET 954
#define MSG_OPTIONS_HVOFFSET2 955
#define MSG_OPTIONS_VOFFSET 956
#define MSG_OPTIONS_PRINTER 957
#define MSG_OPTIONS_OPTIMIZE 958
#define MSG_OPTIONS_DRAFT 959
#define MSG_OPTIONS_DENSITY 960
#define MSG_OPTIONS_DENSITY2 961
#define MSG_OPTIONS_UNIDIRECT 962
#define MSG_OPTIONS_COPIES 963
#define MSG_OPTIONS_LANDSCAPE 964
#define MSG_OPTIONS_TWOPAGE 965
#define MSG_OPTIONS_MOFFSET 966
#define MSG_OPTIONS_BOOK 967
#define MSG_OPTIONS_IFF 968
#define MSG_OPTIONS_SKIPFORMFEED 969
#define MSG_OPTIONS_REVERSE 970
#define MSG_OPTIONS_RESOLUTION 971
#define MSG_OPTIONS_RESOLUTION2 972
#define MSG_OPTIONS_WIDTH 973
#define MSG_OPTIONS_HEIGHT 974
#define MSG_OPTIONS_PRELOAD 975
#define MSG_OPTIONS_FAST 976
#define MSG_OPTIONS_FAST2 977
#define MSG_OPTIONS_MARK 978
#define MSG_OPTIONS_STATISTIC 979
#define MSG_OPTIONS_DEBUGSTAT 980
#define MSG_OPTIONS_NOLOG 981
#define MSG_OPTIONS_OUTTO 982
#define MSG_OPTIONS_OUTTO2 983
#define MSG_OPTIONS_LOGNAME 984
#define MSG_OPTIONS_SHOWPRINTERS 985
#define MSG_OPTIONS_ACCOUNTING 986
#define MSG_OPTIONS_PRIORITY 987
#define MSG_OPTIONS_SPECIALHOST 988
#define MSG_OPTIONS_PRINTAUTHOR 989
#define MSG_OPTIONS_DVIFILE 990
#define MSG_OPTIONS_PUBSCREEN 991
#define MSG_OPTIONS_PAPER 992
#define MSG_OPTIONS_PAPER2 993
#define MSG_MUI_MW_DESCRIPTION 994
#define MSG_MUI_MW_TITEL 995
#define MSG_MUI_MW_DVIFILE 996
#define MSG_MUI_MW_ASL_DVIFILE 997
#define MSG_MUI_MW_SEITEN 998
#define MSG_MUI_MW_VON 999
#define MSG_MUI_MW_BIS 1000
#define MSG_MUI_MW_NUM 1001
#define MSG_MUI_MW_KOPIEN 1002
#define MSG_MUI_MW_DRUCKEN 1003
#define MSG_MUI_MW_PREF 1004
#define MSG_MUI_MW_CANCEL 1005
#define MSG_MUI_MW_APP_FAIL 1006
#define MSG_MUI_MW_SEITEN_ALL 1007
#define MSG_MUI_MW_SEITEN_VONBIS 1008
#define MSG_MUI_ME_WINTITLE 1009
#define MSG_MUI_ME_CLOSE 1010
#define MSG_MUI_ME_TITEL 1011
#define MSG_MUI_WO_WINTITLE 1012
#define MSG_MUI_WO_TITEL 1013
#define MSG_MUI_WO_SEEMESS 1014
#define MSG_MUI_WORK_DOALL 1015
#define MSG_MUI_WORK_START 1016
#define MSG_MUI_WORK_END 1017
#define MSG_MUI_WORK_FROM 1018
#define MSG_MUI_WORK_PAGE 1019
#define MSG_MUI_WORK_TO 1020
#define MSG_MUI_FA_WINTITLE 1021
#define MSG_MUI_FA_TITEL 1022

#endif /* CATCOMP_NUMBERS */


/****************************************************************************/


#ifdef CATCOMP_STRINGS

#define MSG_CATALOG_VERSION_STR "141.2"
#define MSG_WRONG_CATALOG_VERSION_STR "Wrong catalog version!!\nUse builtin texts!"
#define MSG_PROJECT_MENU_STR "ShowDVI"
#define MSG_OUTFIT_MENU_STR "Outfit"
#define MSG_MOVE_MENU_STR "Move"
#define MSG_RESOLUTION_MENU_STR "Resolution"
#define MSG_PROJECT_ABOUT_STR "About ..."
#define MSG_PROJECT_OPENAGAIN_STR "Open again"
#define MSG_PROJECT_OPENNEW_STR "Open new ..."
#define MSG_PROJECT_AUTOLOADAGAIN_STR "Auto load again"
#define MSG_PROJECT_SAVEIFF_STR "Save as IFF file ..."
#define MSG_PROJECT_SHELLCOMANDS_STR "Shell commands"
#define MSG_PROJECT_PRINTPAGE_STR "Print/cancel print page"
#define MSG_PROJECT_WBTOFRONT_STR "WB to front"
#define MSG_PROJECT_HIDE_STR "Hide"
#define MSG_PROJECT_SAVECONFIG_STR "Save config"
#define MSG_PROJECT_QUIT_STR "Quit"
#define MSG_PROJECT_SHELLCOMANDS_NEWCLI_STR "New CLI"
#define MSG_PROJECT_SHELLCOMANDS_EXECUTECOMMAND_STR "Execute Command"
#define MSG_PROJECT_SHELLCOMANDS_TEXSCRIPT_STR "TeX-Script"
#define MSG_PROJECT_SHELLCOMANDS_AREXXTEXSHELL_STR "ARexx TeX-Shell"
#define MSG_PROJECT_SHELLCOMANDS_SETENVTEXFORMAT_STR "Set ENV:TEXFORMAT"
#define MSG_PROJECT_SHELLCOMANDS_SPECIALHOST_STR "SpecialHost"
#define MSG_OUTFIT_COPY_STR "Copy"
#define MSG_OUTFIT_LACE_STR "Lace"
#define MSG_OUTFIT_SCROLLBAR_STR "Scrollbar"
#define MSG_OUTFIT_FULLPAGE_STR "Full page"
#define MSG_OUTFIT_MEASUREWINDOW_STR "Measure window"
#define MSG_OUTFIT_BORDERLINE_STR "Border line"
#define MSG_OUTFIT_SETMARGIN_STR "Set margins"
#define MSG_OUTFIT_4COLORSCREEN_STR "4 color screen"
#define MSG_OUTFIT_PAGESCROLLBAR_STR "Page scrollbar"
#define MSG_OUTFIT_UNIT_STR "Unit"
#define MSG_OUTFIT_COLOR_STR "Color"
#define MSG_OUTFIT_CLONEWBCOLOR_STR "Clone WB color"
#define MSG_OUTFIT_SCREENPREFS_STR "Screen Prefs ..."
#define MSG_OUTFIT_UNIT_INCH_STR "inch"
#define MSG_OUTFIT_UNIT_CM_STR "cm"
#define MSG_OUTFIT_UNIT_PT_STR "pt"
#define MSG_MOVE_SEARCH_STR "Search string ..."
#define MSG_MOVE_PREVPAGE_STR "Prev page"
#define MSG_MOVE_NEXTPAGE_STR "Next page"
#define MSG_MOVE_FIRSTPAGE_STR "First page"
#define MSG_MOVE_LASTPAGE_STR "Last page"
#define MSG_MOVE_PAGECOUNTER_STR "== Page Counter =="
#define MSG_MOVE_JUMPTOPAGENUMBER_STR "Jump to page number"
#define MSG_MOVE_CLEARPAGECOUNTER_STR "Clear page counter"
#define MSG_MOVE_USEPHY_STR "== Use Pysical Pagenumbers =="
#define MSG_MOVE_USEORDERDVI_STR "Use order of the dvi-file"
#define MSG_MOVE_USEPHYPREVPAGE_STR "Prev page"
#define MSG_MOVE_USEPHYNEXTPAGE_STR "Next page"
#define MSG_MOVE_USEPHYFIRSTPAGE_STR "First page"
#define MSG_MOVE_USEPHYLASTPAGE_STR "Last page"
#define MSG_NO_LIBRARY_STR "Can't open library '%s' (version >= %ld)!"
#define MSG_NO_CONSOLE_DEVICE_STR "Can't open the \"console.device\"?!"
#define MSG_NO_NEW_OBJECT_STR "Can't get new object '%s'!"
#define MSG_CANT_CLOSE_SCREEN_STR "Can't close the Screen!\nPlease close all windows\nand send a ^C to me."
#define MSG_CLOSE_WINDOWS_STR "You must first close all your windows!"
#define MSG_SCRERR_NOERR_STR "no error?"
#define MSG_SCRERR_NOMONITOR_STR "monitor for display mode not available"
#define MSG_SCRERR_NOCHIPS_STR "you need newer custom chips for display mode"
#define MSG_SCRERR_NOMEM_STR "couldn't get normal memory"
#define MSG_SCRERR_NOCHIPMEM_STR "couldn't get chip memory"
#define MSG_SCRERR_PUBNOTUNIQUE_STR "public screen name already used"
#define MSG_SCRERR_UNKNOWNMODE_STR "don't recognize display mode requested"
#define MSG_SCRERR_DEFAULT_STR "unknown error"
#define MSG_CANT_OPEN_SCR_NAME_STR "can't open screen: \"%s\"!"
#define MSG_CANT_FIND_SCR_USE_WB_STR "Can't find screen \"%s\"! Will try workbench screen..."
#define MSG_CANT_LOCK_PBSCR_STR "Can't lock public-screen!"
#define MSG_CANT_GET_VI_STR "Can't get visual info!"
#define MSG_CANT_GET_DI_STR "Can't get draw info!"
#define MSG_CANT_OPEN_WIN_STR "Can't open window!"
#define MSG_SET_MARGIN_STR "Top/left corner (for ENTER...) set."
#define MSG_SEARCH_STRING_NOT_FOUND_STR "Searchstring \"%s\" not found."
#define MSG_SEARCH_STRING_CANCELED_STR "Search canceled."
#define MSG_SHOWDVI_MESSAGE_STR "ShowDVI message "
#define MSG_ABORT_PRINT_PAGE_STR "Abort print page?"
#define MSG_OK_CANCEL_REQSTRING_STR "Ok|Cancel"
#define MSG_ABORT_PRINT_STR "Abort printing..."
#define MSG_PRINT_IS_ABORTED_STR "Print aborted!"
#define MSG_PRINT_CUR_PAGE_STR "Print current page."
#define MSG_WIN_HEADER_FILE_STR "ShowDVI   File: %s   Page: %d / %d  "
#define MSG_WIN_HEADER_NO_FILE_STR "ShowDVI V%s%c  «no file»  "
#define MSG_UNAVAILABLE_MODE_STR "Unavailable mode"
#define MSG_UNKNOWN_MODE_NAME_STR "Unknown mode name"
#define MSG_INTERNAL_ERROR_STR "Internal error!"
#define MSG_NO_CHIPMEM_STR "Not enough chip-memory!"
#define MSG_BUILD_FULL_PAGE_STR "Building full page..."
#define MSG_NO_MEM_STR "Not enough memory!"
#define MSG_CANT_SAVE_TO_CLIP_STR "Can't save page to the clipboard!"
#define MSG_CANT_SAVE_TO_IFF_STR "Can't save page to iff-file \"%s\"!"
#define MSG_NO_SIGNAL_STR "No signal available!"
#define MSG_CANT_NOTIFY_STR "Notify of file '%s' don't work!\n%s\nI use own notify routine."
#define MSG_MARGIN_SET_STR "Top/left margin set."
#define MSG_SHOWDVI_ALREADY_ACTIVE_STR "ShowDVI already running! (You can't set any options)\n"
#define MSG_CANT_ACCESS_FILE_STR "Can't access file '%s'!"
#define MSG_AREXX_COMM_START_FAILED_STR "Start of ARexx-command failed!"
#define MSG_AREXX_COMM_FAILED_STR "ARexx comamnd '%s' failed with return code %d!"
#define MSG_DIV_ZERO_STR "Division by zero!!"
#define MSG_CANT_SET_VARIABLE_STR "Can't set ENV: variable \"%s\"!"
#define MSG_ERROR_IN_CONFIG_FILE_STR "There are problems in the config-file!"
#define MSG_BETA_VERSION_STR "Attention:\nThis is a beta-test version!"
#define MSG_INTERNAL_ERROR_W_ARG_STR "Internal error: \"%s\"!"
#define MSG_WRONG_MAP_WIDTH_STR "incorrect Map-width"
#define MSG_NO_DVI_FILE_STR "No DVI-File loaded!"
#define MSG_DVI_FILE_LOST_STR "Where is my DVI-File?!"
#define MSG_UNKNOWN_POP_SUBMENU_STR "Unknown sub-menu!?"
#define MSG_FILENAME_STR "Filename:"
#define MSG_ENTER_COMMAND_STR "Enter Command and its Arguments"
#define MSG_EXECUTE_COMMAND_STR "Execute Command Output"
#define MSG_CANT_EXECUTE_COMMAND_STR "Can't execute command!"
#define MSG_USE_WHICH_PUBSCR_STR "Use which Public-Screen?"
#define MSG_DONT_FIND_PUBSCR_USE_WB_STR "Don't find Public-Screen \"%s\"!\nCould I use Workbench-Screen?"
#define MSG_SHOWDVI_TEX_SHELL_STR "ShowDVI TeX-Shell"
#define MSG_CANT_START_STR "Can't start \"%s\"!"
#define MSG_START_SPECIALHOST_STR "Start SpecialHost"
#define MSG_UNKNOWN_SUBITEM_STR "Unknown SubItem (%d, %d, %d)"
#define MSG_UNKNOWN_SUBMENU_STR "Unknown SubMenu (%d, %d, %d)"
#define MSG_UNKNOWN_MENU_STR "Unknown Menu (%d, %d, %d)"
#define MSG_CANT_OPEN_HELP_FILE_STR "Can't open the help-file \"%s\"!"
#define MSG_SHOWDVI_MENU_HELP_STR "ShowDVI Menu Help"
#define MSG_NO_HELP_FOR_MENU_STR "Found no help for menu (%d, %d, %d)!?"
#define MSG_INTERNAL_ERROR_MENU_ENTRY_STR "Internal error: can't find Menu-Entry (%d, %d, %d) [%d]"
#define MSG_CONFUSED_ABOUT_REQ_STR "Confused about requester!"
#define MSG_SHOWDVI_ABOUT_WINDOW_STR "ShowDVI About/Help Window"
#define MSG_CANT_OPEN_ABOUT_REQ_STR "Can't open About-Requester window!"
#define MSG_NEED_REQ_LIB_STR "I need the 'req.library'!"
#define MSG_SHOWDVI_REQUEST_STR "ShowDVI Request"
#define MSG_OK_STR "OK"
#define MSG_CANCEL_STR "Cancel"
#define MSG_SHOWDVI_FATAL_MESSAGE_STR "ShowDVI fatal message"
#define MSG_FATAL_WRONG_STR "Something is wrong...\n\n%s\n\nBetter I quit!"
#define MSG_FATAL_CHIPMEM_STR "Sorry, not enough ChipMem...\n\n%s\n\nI must quit!"
#define MSG_FATAL_MEMORY_STR "Lack of memory?\n\n%s\n\nProgram terminate!"
#define MSG_FATAL_INTERNAL_STR "Internal error...\n\n%s\n\nProgram terminate!"
#define MSG_FATAL_FATAL_STR "Program is totally confused!\n\n%s\n\nBetter I quit!"
#define MSG_REALLY_QUIT_STR "Really quit?"
#define MSG_YES_NO_STR "Yes|No"
#define MSG_FILE_ISNT_DVI_FILE_STR "The file \"%s\" isn't a DVI-File!"
#define MSG_CANT_ALLOC_FREQ_STR "can't allocate filerequest-structure"
#define MSG_LOAD_NEW_DVI_STR "Load new DVI-File"
#define MSG_CANT_FOUND_FILE_STR "Can't found file \"%s\"!"
#define MSG_CANT_ALLOC_ASLREQ_STR "Can't initialize AslRequest!"
#define MSG_FORMAT_FILE_NAME_STR "Please input your format-file name."
#define MSG_NOT_WHILE_PRINTING_STR "This is not allowed while printing!"
#define MSG_NO_APP_ICON_STR "Can't setup application icon!"
#define MSG_ENTER_A_STRING_STR "Enter a string"
#define MSG_ENTER_A_NUMBER_STR "Enter a number"
#define MSG_CANT_OPEN_CONFIG_STR "*** Can't open ShowDVI configuration file \"%s\"!"
#define MSG_LOAD_CONFIG_STR "Loading ShowDVI configuration file \"%s\"."
#define MSG_WRITE_CONFIG_STR "Writing ShowDVI configuration file \"%s\"."
#define MSG_NO_AREXX_FOR_FX_STR "No ARexx command for function key %d!"
#define MSG_NO_ON_OFF_STR "missing 'on' or 'off'!"
#define MSG_WRONG_ON_OFF_STR "read '%s' instead of 'on' or 'off'!"
#define MSG_UNKNOWN_KEYWORD_STR "Unknown keyword '%s' in config-file!"
#define MSG_UNKNOWN_MODEID_STR "ModeID %x not available!"
#define MSG_ILLEGAL_COLOR_STR "Illegal color value!"
#define MSG_WRONG_COLOR_PARAMS_STR "Error in colors parameter!"
#define MSG_WRONG_DEF_RESO_STR "Error in parameters for default resolution"
#define MSG_WRONG_VAL_IN_RES_MENU_STR "Strange value for the resolution-menu!"
#define MSG_WRONG_UNITS_STR "Read '%s' instead of 'cm', 'in' or 'pt'!"
#define MSG_WRONG_APPICON_POS_STR "Illegal app-icon position \'%s\'!"
#define MSG_WRONG_MIN_SCREEN_SIZE_STR "Illegal screen size \'%s\' (min: 540x200)!"
#define MSG_WRONG_SCREEN_SIZE_STR "Illegal screen size \'%s\'!"
#define MSG_WRONG_MONITOR_SIZE_STR "Illegal monitor size \'%s\'!"
#define MSG_WRONG_WIN_POS_STR "Illegal window position \'%s\'!"
#define MSG_WRONG_WIN_SIZE_STR "Illegal window size \'%s\'!"
#define MSG_WRONG_MAX_DVIBUF_SIZE_STR "Illegal maximal dvibuf size \'%s\'!"
#define MSG_CONFIG_HEADER_STR "\; This is a machine generated configuration file for ShowDVI V%s\n"
#define MSG_CONFIG_RGB_BACK_STR "RGB values of background color"
#define MSG_CONFIG_RGB_FORE_STR "RGB values of foreground color"
#define MSG_CONFIG_RGB_2_STR "RGB values of color 2"
#define MSG_CONFIG_RGB_3_STR "RGB values of color 3"
#define MSG_CONFIG_SCROLL_STATE_STR "state of scrollbars"
#define MSG_CONFIG_BORDER_LINE_STR "draw border line"
#define MSG_CONFIG_LACE_STR "interlace"
#define MSG_CONFIG_BEEP_STR "beep on warnings"
#define MSG_CONFIG_ESC_STR "exit prog on ESC"
#define MSG_CONFIG_QUICK_EXIT_STR "exit prog immediately"
#define MSG_CONFIG_POPUP_STR "use pop-up-menu"
#define MSG_CONFIG_INT_POPUP_STR "is the pop-up-menu \"intelligent\""
#define MSG_CONFIG_BIG_POPUP_STR "pop-up-menu with topaz 11"
#define MSG_CONFIG_MIDDLE_POPUP_STR "use middle-mouse-button for pop-up menu"
#define MSG_CONFIG_USEPHY_STR "on == use physical numbering"
#define MSG_CONFIG_ACT_LOAD_AGAIN_STR "activate window => load file again"
#define MSG_CONFIG_USE_OWN_SCR_STR "use own screen"
#define MSG_CONFIG_SHOW_PUBSCR_NAME_STR "name of the ShowDVI public screen (default: ShowDVI-PubScr)"
#define MSG_CONFIG_DEF_PUBSCR_NAME_STR "name of the public screen (default: Workbench)"
#define MSG_CONFIG_WIN_POS_STR "position of the main window (x, y) (-1, -1 default pos)"
#define MSG_CONFIG_WIN_SIZE_STR "size of the main window (x, y) (-1, -1 default size)"
#define MSG_CONFIG_SCR_POS_STR "position at the ShowDVI screen (x, y) (-1, -1 default pos)"
#define MSG_CONFIG_SHOW_WIN_SIZE_STR "window size at ShowDVI screen (x, y) (-1, -1 default size)"
#define MSG_CONFIG_4_COL_SCR_STR "use 4 color screen"
#define MSG_CONFIG_BACK_HOOK_STR "use color 2 as background"
#define MSG_CONFIG_USE_WB_COLS_STR "clone the wb colors"
#define MSG_CONFIG_UNIT_STR "used unit"
#define MSG_CONFIG_APP_ICON_STR "application-icon on/off"
#define MSG_CONFIG_INFO_APP_ICON_STR "\".info\" file for the app-icon"
#define MSG_CONFIG_APP_ICON_POS_STR "position 0,0 => use default position"
#define MSG_CONIFG_SCRIPT_FILE_STR "Script-file (only for OS 2.0)"
#define MSG_CONFIG_TEX_SERVER_STR "Script-file, start the TeX-server (only for OS 2.0)"
#define MSG_CONFIG_SCR_SIZE_STR "screen size. 0,0 => clone wb-size"
#define MSG_CONFIG_SCR_MODE_STR "screen mode (pal|ntsc|productivity|a2024|workbench) or mode-id"
#define MSG_CONFIG_MONITOR_SIZE_STR "monitor size (x, y)"
#define MSG_CONFIG_ALWBMFAST_STR "use always fast-ram for the bitmap (slower scrolling)"
#define MSG_CONFIG_SMARTWIN_STR "use 'smart-refresh' window (needs more chip-mem, faster)"
#define MSG_CONFIG_MAX_DVIBUF_STR "what's the max size for caching the whole DVI file in ram?"
#define MSG_CONFIG_DEF_RESO_STR "default start resolution"
#define MSG_CONFIG_RESO_MENU_STR "entries for the resolution menu"
#define MSG_CONFIG_SAVED_OK_STR "Configuration saved."
#define MSG_CANT_SAVE_CONFIG_STR "Can't save configuration!"
#define MSG_CONFIG_APEN_STR "Pen number for foreground color (0..255)"
#define MSG_CONFIG_BPEN_STR "Pen number for background color (0..255)"
#define MSG_WRONG_PEN_STR "Error in color-pen definition!"
#define MSG_MEASURE_WINDOW_STR "Measure Window"
#define MSG_CANT_OPEN_MESSWIN_STR "Can't open MessWin!"
#define MSG_MESSWIN_WIDTH_STR "Width:"
#define MSG_MESSWIN_HEIGHT_STR "Height:"
#define MSG_MESSWIN_DEL_X_STR "Delta-X:"
#define MSG_MESSWIN_X_STR "X-Koo:  "
#define MSG_MESSWIN_DEL_Y_STR "Delta-Y:"
#define MSG_MESSWIN_Y_STR "Y-Koo:  "
#define MSG_MESSWIN_MAG_STR "Magnification: %4d"
#define MSG_AREXX_REPLY_CODE_STR "ARexx reply code '%ld'!"
#define MSG_EXECUTE_TEX_SCRIPT_STR "ShowDVI - Execute TeX-Script"
#define MSG_NEWCLI_TITLE_STR "ShowDVI - new CLI"
#define MSG_PREFWIN_GADARR_0_STR "Screen _Width:"
#define MSG_PREFWIN_GADARR_1_STR "Screen _Height:"
#define MSG_PREFWIN_GADARR_2_STR "Back_ground:"
#define MSG_PREFWIN_GADARR_3_STR "_Overscan Size"
#define MSG_PREFWIN_GADARR_4_STR "_DVI-Page Size"
#define MSG_PREFWIN_GADARR_5_STR "_Use"
#define MSG_PREFWIN_GADARR_6_STR "_Cancel"
#define MSG_PREFWIN_GADARR_7_STR "Use Own _Screen"
#define MSG_PREFWIN_GADARR_8_STR "_4 Col Screen:"
#define MSG_PREFWIN_GADARR_9_STR "Change Screen _Mode"
#define MSG_PREFWIN_GADARR_10_STR "Screen Mode:"
#define MSG_PREFWIN_GADTOOLS_STR "I need the gadtools.library version 37.x or higher.\nPlease update your operating system software!"
#define MSG_PREFWIN_WIN_TITLE_STR "Screen Preferences"
#define MSG_PREFWIN_CANT_OPEN_STR "Can't open Screen Preference window."
#define MSG_SEARCHWIN_WIN_TITLE_STR "Insert search text..."
#define MSG_SEARCHWIN_DOSEARCH_STR "Search in progress..."
#define MSG_SEARCHWIN_FOUND_STR "Text found..."
#define MSG_SEARCHWIN_CANT_OPEN_STR "Can't open search window."
#define MSG_SEARCHWIN_STRING_STR "Strin_g to search:"
#define MSG_SEARCHWIN_SEARCH_STR "_search"
#define MSG_SEARCHWIN_CANCEL_STR "_cancel"
#define MSG_NO_FILE_GIVEN_STR "No file specified!"
#define MSG_BUILD_BITMAP_STR "Building bitmap ..."
#define MSG_STACK_OVER_STR "stack overflow"
#define MSG_STACK_UNDER_STR "stack underflow"
#define MSG_PRE_IN_FILE_STR "PRE occurs within file"
#define MSG_POST_IN_FILE_STR "Reached POST command in DVI file!"
#define MSG_POST_POST_STR "POST_POST with no preceding POST"
#define MSG_DVI_FILE_ERROR_STR "DVI file error!"
#define MSG_ALREADY_FIRST_STR "You are already on the first page."
#define MSG_ALREADY_LAST_STR "You are already on the last page."
#define MSG_PAGE_NOT_FOUND_STR "page %d not found!"
#define MSG_MISSING_PRE_STR "PRE doesn't occur first--are you sure this is a DVI file?"
#define MSG_WRONG_DVI_TYPE_STR "DVI format = %d, can only process DVI format %d files"
#define MSG_LOADING_DVI_STR "Loading DVI file \"%s\"."
#define MSG_INCOMPLETE_DVI_PRELOAD_STR "Incomplete DVI file: can't preload fonts!"
#define MSG_SCAN_DVI_FILE_STR "Scanning DVI file ..."
#define MSG_HELP_HELP_STR "print help information"
#define MSG_HELP_FONTDIR_STR "additional dir for fontlibs/pk-files"
#define MSG_HELP_FONTMEM_STR "size of the fontmemory"
#define MSG_HELP_FROM_STR "start at page"
#define MSG_HELP_HOFFSET_STR "horizontal offset `num'[true]`unit'"
#define MSG_HELP_HOFFSET_2_STR "(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)"
#define MSG_HELP_VOFFSET_STR "vertical offset `num'[true]`unit'"
#define MSG_HELP_VOFFSET_2_STR "(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)"
#define MSG_HELP_PRELOAD_STR "preload all fonts"
#define MSG_HELP_RESOLUTION_STR "starting resolution in DPI"
#define MSG_HELP_STATISTIC_STR "more output to the logfile"
#define MSG_HELP_DEBUGSTAT_STR "close logfile after every line"
#define MSG_HELP_LOGNAME_STR "logfile name"
#define MSG_HELP_NOLOG_STR "don't create logfile"
#define MSG_HELP_PRIORITY_STR "set the task priority"
#define MSG_HELP_PRINTAUTHOR_STR "show author name"
#define MSG_HELP_DVIFILE_STR "DVI-file"
#define MSG_COPYRIGHT_STR "Last compiled: %s"
#define MSG_USAGE_STR "usage: %s "
#define MSG_CANT_FIND_FILE_STR "Can't find DVI-file \"%s\""
#define MSG_NOT_ENOUGH_MEM_BYTES_STR "not enough memory (%d bytes)!"
#define MSG_PROGRAM_END_ERR_STR "\nProgram aborted with error code %d!"
#define MSG_USER_ABORT_STR "\nUser aborted program!"
#define MSG_PROGRAM_END_OK_STR "\nProgram terminated without errors."
#define MSG_DVIP_PRINT_FINISHED_STR "\nPrint finished."
#define MSG_FATAL_STR "FATAL-- "
#define MSG_LOG_FILE_CREATED_STR "Log file (%s) created."
#define MSG_LINE_BUFFER_OVERFLOW_STR "Line buffer overflow! (%d)"
#define MSG_BREAK_STR "*** BREAK"
#define MSG_BREAK_IO_STR "*** BREAK (wait for IO to complete)"
#define MSG_CALL_MF_STR "Call Metafont via \"%s\"."
#define MSG_CANT_OPEN_STR "Can't open \"%s\"!\n"
#define MSG_DVIP_WHOLE_BITMAP_IN_RAM_STR "In this print mode the whole bitmap must be in the memory!"
#define MSG_CANT_ALLOC_BITMAP_STR "Can't allocate enough memory for bitmap!"
#define MSG_DVIP_PARTS_STR "page is splitted in %d parts"
#define MSG_DVIP_SAVE_CLIP_STR "Save page number %d of file %s to the clipboard %s"
#define MSG_DVIP_SAVE_IFF_STR "Save page number %d of file %s to %s"
#define MSG_DVIP_CANT_SAVE_IFF_STR "Can't write page to iff-file \"%s\"!"
#define MSG_SPECIAL_TOO_LONG_STR "\\special too long -- ignored."
#define MSG_NO_MEM_FOR_SPECIAL_STR "Can't allocate memory for \\special -- ignored."
#define MSG_PICT_OUT_LEFT_STR "Picture out of left border! (x=%ld)"
#define MSG_PICT_OUT_RIGHT_STR "Picture out of right border!"
#define MSG_NO_MEM_FOR_SPECIAL_BITMAP_STR "No memory for special-bitmap copy!"
#define MSG_UNKNOWN_PICT_LOC_STR "Unknown picture locating!?"
#define MSG_ILLEG_PS_COMM_STR "Illegal .ps command format: %s"
#define MSG_TOO_MANY_POINTS_STR "Too many points defined - ignored %s"
#define MSG_MALFORMED_PATH_COMM_STR "Malformed path command - ignored %s"
#define MSG_BAD_DVI_FILE_END_STR "Bad end of DVI file"
#define MSG_BAD_FONT_DEFS_STR "Bad byte value in font defs"
#define MSG_MISS_POST_POST_STR "POST_POST missing after fontdefs"
#define MSG_MISS_POST_STR "POST missing at head of postamble - DVI file not complete"
#define MSG_LOG_PAGE_DIM_STR "\nPage dimensions:"
#define MSG_LOG_HORIZ_STR " horizontal: %2.2fin, %4d pixels, %3lu points, %8ld DVI units,"
#define MSG_LOG_VERT_STR " vertical  : %2.2fin, %4d pixels, %3lu points, %8ld DVI units."
#define MSG_LOG_MAG_STR "Magnification = %ld; %ld pixels per DVI unit."
#define MSG_LOG_OFFSET_STR "Offsets:"
#define MSG_LOG_THOFF_STR " horizontal: %2.2fin (%2.1ftruein), %4d pixels,"
#define MSG_LOG_HOFF_STR " horizontal: %2.2fin (%2.1fin), %4d pixels,"
#define MSG_LOG_TVOFF_STR " vertical  : %2.2fin (%2.1ftruein), %4d pixels."
#define MSG_LOG_VOFF_STR " vertical  : %2.2fin (%2.1fin), %4d pixels."
#define MSG_LOG_RESO_STR "Horizontal/vertical resolution: %d / %d dpi."
#define MSG_DVI_STACK_TOO_SMALL_STR "DVI stack too small, can only handle size %d!\n"
#define MSG_LOG_NUM_PAGES_STR "%d pages in document.\n"
#define MSG_CANT_OPEN_FONTLIB_STR "*** Can't open library \"%s\"!"
#define MSG_NOT_A_FONTLIB_STR "*** File \"%s\" is no font library!"
#define MSG_OLD_FONTLIB_STR "Warning: File \"%s\" is an old font library!"
#define MSG_TOO_MANY_FONTLIB_LEVELS_STR "*** To many link levels!"
#define MSG_ERROR_READING_FONTLIB_DIR_STR "*** Error during reading flib-directory!"
#define MSG_BAD_PK_FILE_STR "Bad PK file %s: %s"
#define MSG_UNEXPECTED_EOF_IN_PK_STR "unexpected eof"
#define MSG_EXPECTED_PRE_IN_PK_STR "expected pre"
#define MSG_WRONG_ID_IN_PK_STR "wrong id byte"
#define MSG_CHECKSUM_MISS_IN_PK_STR "Checksum mismatch"
#define MSG_CHAR_CODE_OUT_OF_RANGE_IN_PK_STR "character code out of range"
#define MSG_PACKET_LENGTH_SMALL_IN_PK_STR "packet length too small"
#define MSG_NO_MEM_FOR_CHAR_STR "Not enough memory for char"
#define MSG_UNEXPECTED_COMM_IN_PK_STR "! unexpected command"
#define MSG_RELEASE_CHARS_STR "Release all unpacked chars (%ld bytes for %ld chars)"
#define MSG_INTERNAL_ERROR_SPRINTF_STR "internal error in my_sprintf!"
#define MSG_UNKNOWN_FORMAT_SPRINTF_STR "unknown format spec in format-string!"
#define MSG_STRING_OVERFLOW_SPRINTF_STR "string overflow in my_sprintf!"
#define MSG_ERROR_SEARCH_LIST_LIB_STR "Error while reading search list for font libraries!"
#define MSG_ERROR_SEARCH_LIST_PK_STR "Error while reading search list for PK files!"
#define MSG_FONT_UNDEFINED_STR "Font %ld is undefined"
#define MSG_FONT_ALREADY_DEFINED_STR "Font %s (%ld) is already defined"
#define MSG_FONT_STR_UNDEFINED_STR "Font %s (%ld) is undefined"
#define MSG_RELOAD_FONT_STR "Reloading font %-10s (%3ld dpi)"
#define MSG_FONTMEM_USED_STR "  \t(font memory used: %3ld%%)"
#define MSG_LOAD_LOADED_FONT_STR "Load a loaded font?? (%s,%ld)\n"
#define MSG_LOAD_FONT_STR "Loading font %-10s (%3ld dpi)."
#define MSG_INTERNAL_ERROR_LIB_MISMATCH_STR "Internal error: library structure mismatched"
#define MSG_LOAD_FONT_MEM_USED_STR "Loading font %-10s (%3ld dpi).   \t(font memory used: %3ld%%)"
#define MSG_FOUND_IN_LIB_STR "\tfound at: dir '%s', library '%s'"
#define MSG_FOUND_AS_PK_STR "\tfound at: dir '%s', pk-file '%s'"
#define MSG_MEMORY_ERROR_STR "memory error!?!"
#define MSG_SUBSTITUTE_FONT_STR "** Substituted font %-10s: %3ld --> %3ld dpi."
#define MSG_FONT_NOT_FOUND_STR "**** Font %-10s (%3ld dpi) not found! ****"
#define MSG_FONT_XY_NOT_FOUND_STR "**** Font %-10s (%3ld x %3ld dpi) not found! ****"
#define MSG_TRY_SUBSTITUTE_STR "** Trying to substitute font %-10s: %3ld --> %3ld dpi."
#define MSG_PREDEF_FONT_IN_LIB_STR "Predefined font in library? This feature is not implemented!"
#define MSG_PREDEF_FONT_NOT_FOUND_STR "**** predefined font %-10s (%3ld dpi) not found!"
#define MSG_INTERNAL_ERROR_FMT_STR_STR "internal error (fmt_str == NULL)!"
#define MSG_BYTES_USED_FOR_CHARS_STR "%ld bytes used for %ld unpacked chars."
#define MSG_SEARCHPATH_LIB_ENTRIES_NR_STR "The search path for fontlibraries has %ld entries:"
#define MSG_SEARCHPATH_PK_ENTRIES_NR_STR "The search path for pk-files has %ld entries:"
#define MSG_LOG_FONTDEF_START_STR "-------- start of fontdef file --------"
#define MSG_LOG_FONTDEF_LIB_FOUND_STR "flib\t\t%lu\t%s\t\t; library found"
#define MSG_LOG_FONTDEF_LIB_DEF_STR "flib\t\t%lu\t%s\t\t; library only defined"
#define MSG_LOG_FONTDEF_LIB_NOT_FOUND_STR "; ** flib\t%lu\t%s\t\t; library *not* found"
#define MSG_LOG_FONTDEF_PK_FOUND_STR "font\t\t%s\t%lu\t%s\t\t; font found"
#define MSG_LOG_FONTDEF_PK_PREDEF_STR "font\t\t%s\t%lu\t%s\t\t; font predefined"
#define MSG_LOG_FONTDEF_PK_PREDEF_NOT_FOUND_STR "; ** font\t%s\t%lu\t%s\t\t; predefined font *not* found"
#define MSG_LOG_FONTDEF_NR_BASE_PK_DIRS_PREDEF_STR "; %ld base pk-directories are predefined"
#define MSG_LOG_FONTDEF_NO_PK_DIRS_PREDEF_STR "; %ld pk-directories are predefined"
#define MSG_LOG_FONTDEF_END_STR "-------- end of fontdef file --------"
#define MSG_CANT_OPEN_FONT_CONFIG_STR "*** Can't open font/flib configuration file \"%s\"!"
#define MSG_LOAD_FONT_CONFIG_STR "Loading font/flib configuration file \"%s\"."
#define MSG_CANT_PARSE_FONT_CONFIG_LINE_STR "Can't parse line \"%s\"!"
#define MSG_TOO_FEW_ARGS_STR "Too few arguments for the \"%s\" command!"
#define MSG_NOT_A_VALID_INT_STR "Not a valid integer \"%s\"!"
#define MSG_CANT_READ_DPI_FONT_COMM_STR "Can't read dpi of the \"font\" command!"
#define MSG_UNKNOWN_KEYWORD_IN_CONFIG_STR "Unknown keyword \"%s\" in font configuration file!"
#define MSG_FONT_REMOVED_STR "Font %-10s (%3ld dpi) removed."
#define MSG_FONT_REMOVED_USED_STR "Font %-10s (%3ld dpi) removed.   \t(font memory used: %3ld%%)"
#define MSG_ERROR_IN_FONTVOLS_STR "Error in \"fontvols\" file. Missing %s! (line %d)"
#define MSG_FONTVOLS_FIRST_DOT_STR "first `.'"
#define MSG_COPY_FONT_TO_CACHE_STR "Copy font \"%s\" (%ld dpi) to font cache \"%s\"."
#define MSG_CANT_COPY_FONT_TO_CACHE_STR "Can't copy pk-font to font cache!"
#define MSG_PARSE_BAD_NR_ARGS_STR "Bad number of arguments line %d: \"%s\"."
#define MSG_PARSE_BAD_ON_OFF_STR "Bad on/off switch in line %d: \"%s\"."
#define MSG_PARSE_CANT_PARSE_STR "Can't parse line %d: \"%s\"."
#define MSG_PARSE_CANT_PARSE_IGNORE_STR "Can't parse line %d: \"%s\", ignoring it."
#define MSG_PARSE_ILLEG_ARG_STR "Illegal argument to keyword '%s' : \"%s\"."
#define MSG_UNPACK_RECURSIV_STR "recursive repeat count in pk file"
#define MSG_UNPACK_MORE_BITS_STR "error while unpacking; more bits than required"
#define MSG_PORT_ALREADY_EXISTS_STR "\"%s\" port already exists!"
#define MSG_CANT_OPEN_PRINTER_PORT_STR "can't open printer port %s!"
#define MSG_CREATEEXTIO_FAILED_STR "\"CreateExtIO\" failed!"
#define MSG_YOU_PRINT_GENERIC_STR "You print with the generic printer:"
#define MSG_GENERIC_PRINT_NAME_STR "  Name : '%s', Version = %d, Revision = %d"
#define MSG_GENERIC_PRINT_RESO_STR "  Resolution: X = %d dpi, Y = %d dpi, density = %d"
#define MSG_GENERIC_PRINT_MAXD_STR "  MaxXDots=%d, MaxYDots=%d."
#define MSG_PRT_ERROR_NO_ERROR_STR "no printer error"
#define MSG_PRT_ERROR_ABORT_STR "printing aborted."
#define MSG_PRT_ERROR_NO_GFX_STR "printer cannot output graphics."
#define MSG_PRT_ERROR_ILLDIM_STR "print dimensions illegal."
#define MSG_PRT_ERROR_NO_MEM_VARS_STR "no memory for internal variables."
#define MSG_PRT_ERROR_NO_MEM_BUFF_STR "no memory for print puffer."
#define MSG_PRT_ERROR_UNKNOWN_ERR_STR "(unknown printer error)"
#define MSG_PRT_ERR_STR "Printer-Error: %s"
#define MSG_TRY_CLEAR_PRT_STR "Trying to clear printer."
#define MSG_CLEAR_FAILED_STR "Zero failed!"
#define MSG_GETOPT_LINEBUF_OVERFLOW_STR "Line overflow in GetOptError!"
#define MSG_GETOPT_ERROR_STR "*** Error: %s"
#define MSG_GETOPT_ERROR_KEY_STR "*** Error: Keyword %.20s (%.50s)"
#define MSG_GETOPT_NO_OPTION_STRING_STR "no option-string given!"
#define MSG_GETOPT_MISSING_NUM_STR "missing number!"
#define MSG_GETOPT_NO_NUMBER_STR "\"%.20s\" is no number!"
#define MSG_GETOPT_MISSING_REAL_STR "missing real number!"
#define MSG_GETOPT_NO_REAL_STR "\"%.20s\" is no real number!"
#define MSG_GETOPT_MISSING_TEX_STR "missing TeX dimension!"
#define MSG_GETOPT_NO_TEX_STR "\"%.20s\" not a TeX dimension!"
#define MSG_GETOPT_UNKNOWN_PARAM_STR "Unknown parameter type!"
#define MSG_GETOPT_WRONG_ONOFF_PARAM_STR "Wrong parameter. '%.20s' instead of (on/off)!"
#define MSG_GETOPT_NO_PARAM_EXPECTED_STR "No parameter expected (%.20s)!"
#define MSG_GETOPT_UNKNOWN_KEYWORD_STR "Unknown keyword \"%.20s\"!"
#define MSG_GETOPT_TOO_MANY_ENV_ARGS_STR "Too many arguments in the environment string!"
#define MSG_GETOPT_NO_MEM_FOR_ENV_STR "No memory for the environment string!"
#define MSG_GETOPT_SUPER_FILE_STR "Supernumerary file \"%.20s\"!"
#define MSG_GETOPT_NO_INFO_STR "Can't get '%s.info'!"
#define MSG_GETOPT_PARAM_REQU_STR "Parameter required!"
#define MSG_GETOPT_TAB_TAB_DEF_STR "\t\t\t%.50s (def.: "
#define MSG_GETOPT_TAB_DEF_STR "\t%.50s%.50s (def.: "
#define MSG_GETOPT_NO_DEFAULT_STR "<no def>"
#define MSG_GETOPT_PRESS_RET_STR "(press <ret> for more help) ? "
#define MSG_SPECIAL_WAIT_FOR_PICT_STR "wait for picture..."
#define MSG_SPECIAL_EXPT_REPLY_SPECIAL_STR "I have expected AC_REPLY_SPECIAL, I found %d!"
#define MSG_SPECIAL_RET_FROM_SPECIAL_STR "Return from \"special\" program: %d!"
#define MSG_SPECIAL_EXPT_REPLY_TPIC_STR "I expected AC_REPLY_TPIC, I found %d!"
#define MSG_SPECIAL_EXPT_REPLY_BITMAP_STR "I expected AC_REPLY_BITMAP, I found %d!"
#define MSG_SPECIAL_PICT_RECEIVED_STR "picture received"
#define MSG_SPECIAL_FOUND_NO_SPECIAL_STR "Found no \"special\" program!"
#define MSG_SPECIAL_CANT_CREATE_PORT_STR "Can't create \"special\" reply-port!"
#define MSG_NO_DVI_FILENAME_STR "No DVI filename given!"
#define MSG_CANT_OPEN_DVI_FILE_STR "Can't open DVI-file '%s'!"
#define MSG_INCOMPLETE_DVI_REVERSE_STR "Incomplete DVI file: can't print in reverse order!"
#define MSG_INCOMPLETE_DVI_STR "Incomplete DVI file!"
#define MSG_UNDEFINED_DVI_COMMAND_STR "%d is an undefined command"
#define MSG_NO_LAND_WITH_GENERIC_STR "No landscape with generic printer!"
#define MSG_NO_LAND_XDPI_YDPI_STR "Different x/y resolution: can't print in landscape modus!"
#define MSG_PRINTER_NAME_STR "\nPrinter \"%s\":"
#define MSG_PRINTER_ID_STR "Printer ID: %s"
#define MSG_MISSING_KEYWORD_STR "\tMissing keyword '%s'."
#define MSG_INCOMPLETE_PRT_DESC_STR "%s: Incomplete printer description ends line %d!"
#define MSG_USE_DRAFT_OPT_STR "Use this with %sdraft, %soptimization."
#define MSG_PRT_RESO_STR "\tresolution %d/%d dpi."
#define MSG_PRT_WIDTH_STR "\tpaper width is %d pica characters."
#define MSG_PRT_PASSES_STR "\tprint in %d pass(es), pin grouping %d"
#define MSG_PRT_BITMAP_HEIGHT_STR "\tbitmap height : %d"
#define MSG_PRT_DEF_BUF_SIZE_STR "\tdefault buffer_size %d."
#define MSG_PRT_MOVE_TO_POINT_STR "\tUse the move-to-point (ESC '$') command."
#define MSG_PRT_ONE_GRAPHIC_COMMAND_STR "\tThe whole line is sent in one graphics command."
#define MSG_PRT_SKIP_WITH_SPACES_STR "\tSkip large white areas with spaces."
#define MSG_PRT_BAD_RESO_STR "%s: Bad resolution in line %d '%s'! "
#define MSG_PRT_USE_RES_STR "Use '<res>' or '<hres>/<vres>'."
#define MSG_UNKNOWN_BLANKING_STR "%s: Unknown blanking method requested in line %d '%s'"
#define MSG_ILLEG_PARM_FOR_KEY_STR "%s: illegal parameter for keyword %s in line %d: %s"
#define MSG_GROUPING_RANGE_STR "%s: Grouping must be in range [1..3] (is %ld)!"
#define MSG_CANT_OPEN_PRT_CONFIG_STR "Can't open printer configuration file \"%s\"!"
#define MSG_READ_PRT_CONFIG_STR "Reading DVIprint printer configuration file \"%s\"."
#define MSG_THATS_ALL_STR "That's all!"
#define MSG_AVAILABLE_PRINTERS_STR "Available printers are :\n"
#define MSG_MAX_GROUPING_STR "Config File: Maximal value for \"grouping\" is 6!"
#define MSG_PRINT_WITH_STR "Printing with : %s"
#define MSG_UNKNOWN_PRT_CONFIG_STR "%s: Unknown printer configuration."
#define MSG_PRT_NO_OUTPUT_REDIRECTION_STR "This printer doesn't allow output redirection."
#define MSG_CANT_OPEN_OUTPUT_FILE_STR "Can't open output file \"%s\"!"
#define MSG_NO_PRT_TYPE_FLAG_STR "Printer type flag not set!"
#define MSG_KEY_TWICE_STR "%s: Keyword specified twice in line %d \"%s\"!"
#define MSG_KEY_CONTEXT_STR "%s: Keyword out of context in line %d \"%s\"!"
#define MSG_CANT_WRITE_OUTPUT_FILE_STR "Can't write to output file!"
#define MSG_WRONG_NUM_COPIES_STR "Wrong number of copies! (1..99)"
#define MSG_PRINT_AT_LEAST_ONE_PAGE_STR "Should I print at least one page or not?"
#define MSG_WRONG_RESO_FORMAT_STR "Wrong resolution format '%s'! Use 'RES nr' or 'RES hnr/vnr'."
#define MSG_NO_ODD_NO_EVEN_STR "Neither 'odd' nor 'even' pages, what's left ?"
#define MSG_DENSITY_RANGE_STR "Density must be in the range [1..7]!"
#define MSG_BITMAP_MEM_MINIMUM_STR "Memory for bitmap set to the minimum: %ld."
#define MSG_TRY_HELP_SHOWP_STR "Please try '%s help'\nand '%s printer help [Showprinters]' first."
#define MSG_UNKNOWN_PAPER_SIZE_STR "Unknown paper size '%s'!"
#define MSG_REVERSE_TWOUP_ERR_STR "Sorry: TWOUP doesn't work together with REVERSE."
#define MSG_PAPER_WIDTH_HEIGHT_ERR_STR "Att: %s overwrites the PAPER option."
#define MSG_OPTIONS_HELP_STR "print help information"
#define MSG_OPTIONS_GUI_STR "usa a graphical user interface"
#define MSG_OPTIONS_FONTDIR_STR "fontlibs & pk-files dir"
#define MSG_OPTIONS_FONTMEM_STR "size of the fontmemory"
#define MSG_OPTIONS_MAXBITMEM_STR "max size of the bitmap"
#define MSG_OPTIONS_MAXBITMEM2_STR "(0 == unlimited size)"
#define MSG_OPTIONS_PRTBUFFER_STR "size of the printer buffer"
#define MSG_OPTIONS_FROM_STR "start at page"
#define MSG_OPTIONS_TO_STR "stop at page"
#define MSG_OPTIONS_NUMBER_STR "number of pages to print"
#define MSG_OPTIONS_ODD_STR "print only all 'odd' pages"
#define MSG_OPTIONS_EVEN_STR "print only all 'even' pages"
#define MSG_OPTIONS_PHYSICAL_STR "from/to numbers are physical numbers"
#define MSG_OPTIONS_HOFFSET_STR "horizontal offset `num'[true]`unit'"
#define MSG_OPTIONS_HVOFFSET2_STR "(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)"
#define MSG_OPTIONS_VOFFSET_STR "vertical offset `num'[true]`unit'"
#define MSG_OPTIONS_PRINTER_STR "printer type"
#define MSG_OPTIONS_OPTIMIZE_STR "optimize printer output?"
#define MSG_OPTIONS_DRAFT_STR "print in draft modus"
#define MSG_OPTIONS_DENSITY_STR "printer density"
#define MSG_OPTIONS_DENSITY2_STR "(only for the 'generic' printer)"
#define MSG_OPTIONS_UNIDIRECT_STR "print unidirectional"
#define MSG_OPTIONS_COPIES_STR "number of copies (only HP printer)"
#define MSG_OPTIONS_LANDSCAPE_STR "print in landscape modus"
#define MSG_OPTIONS_TWOPAGE_STR "print two pages at one paper sheet"
#define MSG_OPTIONS_MOFFSET_STR "offset between the two pages in TWOUP mode"
#define MSG_OPTIONS_BOOK_STR "print pages in book-order (together with TWOUP)"
#define MSG_OPTIONS_IFF_STR "print pages to IFF-files"
#define MSG_OPTIONS_SKIPFORMFEED_STR "no formfeed at the end of last page"
#define MSG_OPTIONS_REVERSE_STR "print in reverse order"
#define MSG_OPTIONS_RESOLUTION_STR "bitmap resolution"
#define MSG_OPTIONS_RESOLUTION2_STR "(Only for special effects! Use RES n or RES h/v)"
#define MSG_OPTIONS_WIDTH_STR "page width (normally not needed)"
#define MSG_OPTIONS_HEIGHT_STR "page height (normally not needed)"
#define MSG_OPTIONS_PRELOAD_STR "preload all fonts"
#define MSG_OPTIONS_FAST_STR "fast-mode"
#define MSG_OPTIONS_FAST2_STR "(needs under 1.3 the 'puffer.device')"
#define MSG_OPTIONS_MARK_STR "mark all used fonts (filenote)"
#define MSG_OPTIONS_STATISTIC_STR "more output to the logfile"
#define MSG_OPTIONS_DEBUGSTAT_STR "close logfile after every line"
#define MSG_OPTIONS_NOLOG_STR "don't create logfile"
#define MSG_OPTIONS_OUTTO_STR "output to file, not to printer"
#define MSG_OPTIONS_OUTTO2_STR "(if IFF and \"OutTo=CLIP\" -> page go to the clipboard)"
#define MSG_OPTIONS_LOGNAME_STR "logfile name"
#define MSG_OPTIONS_SHOWPRINTERS_STR "show printer definitions"
#define MSG_OPTIONS_ACCOUNTING_STR "printer accounting"
#define MSG_OPTIONS_PRIORITY_STR "set the task priority"
#define MSG_OPTIONS_SPECIALHOST_STR "start automatically (if necessary) SpecialHost"
#define MSG_OPTIONS_PRINTAUTHOR_STR "show author name"
#define MSG_OPTIONS_DVIFILE_STR "DVI-file"
#define MSG_OPTIONS_PUBSCREEN_STR "use public screen"
#define MSG_OPTIONS_PAPER_STR "use specific paper size"
#define MSG_OPTIONS_PAPER2_STR "(use something like A3, A4, B3, ...)"
#define MSG_MUI_MW_DESCRIPTION_STR "TeX printer driver for PasTeX"
#define MSG_MUI_MW_TITEL_STR "Print a DVI-File"
#define MSG_MUI_MW_DVIFILE_STR "d DVI-file"
#define MSG_MUI_MW_ASL_DVIFILE_STR "Select a DVI-file"
#define MSG_MUI_MW_SEITEN_STR "g Print pages:"
#define MSG_MUI_MW_VON_STR "f from:"
#define MSG_MUI_MW_BIS_STR "t to:"
#define MSG_MUI_MW_NUM_STR "n Number of pages to print:"
#define MSG_MUI_MW_KOPIEN_STR "# copies:"
#define MSG_MUI_MW_DRUCKEN_STR "_print"
#define MSG_MUI_MW_PREF_STR "p_refs"
#define MSG_MUI_MW_CANCEL_STR "_cancel"
#define MSG_MUI_MW_APP_FAIL_STR "Can't open MUI application!"
#define MSG_MUI_MW_SEITEN_ALL_STR "all"
#define MSG_MUI_MW_SEITEN_VONBIS_STR "from/to"
#define MSG_MUI_ME_WINTITLE_STR "DVIprint messages"
#define MSG_MUI_ME_CLOSE_STR "_close"
#define MSG_MUI_ME_TITEL_STR "Messages"
#define MSG_MUI_WO_WINTITLE_STR "DVIprint print"
#define MSG_MUI_WO_TITEL_STR "Print"
#define MSG_MUI_WO_SEEMESS_STR "_Messages"
#define MSG_MUI_WORK_DOALL_STR "all pages"
#define MSG_MUI_WORK_START_STR "first page"
#define MSG_MUI_WORK_END_STR "last page"
#define MSG_MUI_WORK_FROM_STR "from"
#define MSG_MUI_WORK_PAGE_STR "page"
#define MSG_MUI_WORK_TO_STR "to"
#define MSG_MUI_FA_WINTITLE_STR "Fatal Error"
#define MSG_MUI_FA_TITEL_STR "Fatal Error!"

#endif /* CATCOMP_STRINGS */


/****************************************************************************/


#ifdef CATCOMP_ARRAY

struct CatCompArrayType
{
    LONG   cca_ID;
    STRPTR cca_Str;
};

static const struct CatCompArrayType CatCompArray[] =
{
    {MSG_CATALOG_VERSION,(STRPTR)MSG_CATALOG_VERSION_STR},
    {MSG_WRONG_CATALOG_VERSION,(STRPTR)MSG_WRONG_CATALOG_VERSION_STR},
    {MSG_PROJECT_MENU,(STRPTR)MSG_PROJECT_MENU_STR},
    {MSG_OUTFIT_MENU,(STRPTR)MSG_OUTFIT_MENU_STR},
    {MSG_MOVE_MENU,(STRPTR)MSG_MOVE_MENU_STR},
    {MSG_RESOLUTION_MENU,(STRPTR)MSG_RESOLUTION_MENU_STR},
    {MSG_PROJECT_ABOUT,(STRPTR)MSG_PROJECT_ABOUT_STR},
    {MSG_PROJECT_OPENAGAIN,(STRPTR)MSG_PROJECT_OPENAGAIN_STR},
    {MSG_PROJECT_OPENNEW,(STRPTR)MSG_PROJECT_OPENNEW_STR},
    {MSG_PROJECT_AUTOLOADAGAIN,(STRPTR)MSG_PROJECT_AUTOLOADAGAIN_STR},
    {MSG_PROJECT_SAVEIFF,(STRPTR)MSG_PROJECT_SAVEIFF_STR},
    {MSG_PROJECT_SHELLCOMANDS,(STRPTR)MSG_PROJECT_SHELLCOMANDS_STR},
    {MSG_PROJECT_PRINTPAGE,(STRPTR)MSG_PROJECT_PRINTPAGE_STR},
    {MSG_PROJECT_WBTOFRONT,(STRPTR)MSG_PROJECT_WBTOFRONT_STR},
    {MSG_PROJECT_HIDE,(STRPTR)MSG_PROJECT_HIDE_STR},
    {MSG_PROJECT_SAVECONFIG,(STRPTR)MSG_PROJECT_SAVECONFIG_STR},
    {MSG_PROJECT_QUIT,(STRPTR)MSG_PROJECT_QUIT_STR},
    {MSG_PROJECT_SHELLCOMANDS_NEWCLI,(STRPTR)MSG_PROJECT_SHELLCOMANDS_NEWCLI_STR},
    {MSG_PROJECT_SHELLCOMANDS_EXECUTECOMMAND,(STRPTR)MSG_PROJECT_SHELLCOMANDS_EXECUTECOMMAND_STR},
    {MSG_PROJECT_SHELLCOMANDS_TEXSCRIPT,(STRPTR)MSG_PROJECT_SHELLCOMANDS_TEXSCRIPT_STR},
    {MSG_PROJECT_SHELLCOMANDS_AREXXTEXSHELL,(STRPTR)MSG_PROJECT_SHELLCOMANDS_AREXXTEXSHELL_STR},
    {MSG_PROJECT_SHELLCOMANDS_SETENVTEXFORMAT,(STRPTR)MSG_PROJECT_SHELLCOMANDS_SETENVTEXFORMAT_STR},
    {MSG_PROJECT_SHELLCOMANDS_SPECIALHOST,(STRPTR)MSG_PROJECT_SHELLCOMANDS_SPECIALHOST_STR},
    {MSG_OUTFIT_COPY,(STRPTR)MSG_OUTFIT_COPY_STR},
    {MSG_OUTFIT_LACE,(STRPTR)MSG_OUTFIT_LACE_STR},
    {MSG_OUTFIT_SCROLLBAR,(STRPTR)MSG_OUTFIT_SCROLLBAR_STR},
    {MSG_OUTFIT_FULLPAGE,(STRPTR)MSG_OUTFIT_FULLPAGE_STR},
    {MSG_OUTFIT_MEASUREWINDOW,(STRPTR)MSG_OUTFIT_MEASUREWINDOW_STR},
    {MSG_OUTFIT_BORDERLINE,(STRPTR)MSG_OUTFIT_BORDERLINE_STR},
    {MSG_OUTFIT_SETMARGIN,(STRPTR)MSG_OUTFIT_SETMARGIN_STR},
    {MSG_OUTFIT_4COLORSCREEN,(STRPTR)MSG_OUTFIT_4COLORSCREEN_STR},
    {MSG_OUTFIT_PAGESCROLLBAR,(STRPTR)MSG_OUTFIT_PAGESCROLLBAR_STR},
    {MSG_OUTFIT_UNIT,(STRPTR)MSG_OUTFIT_UNIT_STR},
    {MSG_OUTFIT_COLOR,(STRPTR)MSG_OUTFIT_COLOR_STR},
    {MSG_OUTFIT_CLONEWBCOLOR,(STRPTR)MSG_OUTFIT_CLONEWBCOLOR_STR},
    {MSG_OUTFIT_SCREENPREFS,(STRPTR)MSG_OUTFIT_SCREENPREFS_STR},
    {MSG_OUTFIT_UNIT_INCH,(STRPTR)MSG_OUTFIT_UNIT_INCH_STR},
    {MSG_OUTFIT_UNIT_CM,(STRPTR)MSG_OUTFIT_UNIT_CM_STR},
    {MSG_OUTFIT_UNIT_PT,(STRPTR)MSG_OUTFIT_UNIT_PT_STR},
    {MSG_MOVE_SEARCH,(STRPTR)MSG_MOVE_SEARCH_STR},
    {MSG_MOVE_PREVPAGE,(STRPTR)MSG_MOVE_PREVPAGE_STR},
    {MSG_MOVE_NEXTPAGE,(STRPTR)MSG_MOVE_NEXTPAGE_STR},
    {MSG_MOVE_FIRSTPAGE,(STRPTR)MSG_MOVE_FIRSTPAGE_STR},
    {MSG_MOVE_LASTPAGE,(STRPTR)MSG_MOVE_LASTPAGE_STR},
    {MSG_MOVE_PAGECOUNTER,(STRPTR)MSG_MOVE_PAGECOUNTER_STR},
    {MSG_MOVE_JUMPTOPAGENUMBER,(STRPTR)MSG_MOVE_JUMPTOPAGENUMBER_STR},
    {MSG_MOVE_CLEARPAGECOUNTER,(STRPTR)MSG_MOVE_CLEARPAGECOUNTER_STR},
    {MSG_MOVE_USEPHY,(STRPTR)MSG_MOVE_USEPHY_STR},
    {MSG_MOVE_USEORDERDVI,(STRPTR)MSG_MOVE_USEORDERDVI_STR},
    {MSG_MOVE_USEPHYPREVPAGE,(STRPTR)MSG_MOVE_USEPHYPREVPAGE_STR},
    {MSG_MOVE_USEPHYNEXTPAGE,(STRPTR)MSG_MOVE_USEPHYNEXTPAGE_STR},
    {MSG_MOVE_USEPHYFIRSTPAGE,(STRPTR)MSG_MOVE_USEPHYFIRSTPAGE_STR},
    {MSG_MOVE_USEPHYLASTPAGE,(STRPTR)MSG_MOVE_USEPHYLASTPAGE_STR},
    {MSG_NO_LIBRARY,(STRPTR)MSG_NO_LIBRARY_STR},
    {MSG_NO_CONSOLE_DEVICE,(STRPTR)MSG_NO_CONSOLE_DEVICE_STR},
    {MSG_NO_NEW_OBJECT,(STRPTR)MSG_NO_NEW_OBJECT_STR},
    {MSG_CANT_CLOSE_SCREEN,(STRPTR)MSG_CANT_CLOSE_SCREEN_STR},
    {MSG_CLOSE_WINDOWS,(STRPTR)MSG_CLOSE_WINDOWS_STR},
    {MSG_SCRERR_NOERR,(STRPTR)MSG_SCRERR_NOERR_STR},
    {MSG_SCRERR_NOMONITOR,(STRPTR)MSG_SCRERR_NOMONITOR_STR},
    {MSG_SCRERR_NOCHIPS,(STRPTR)MSG_SCRERR_NOCHIPS_STR},
    {MSG_SCRERR_NOMEM,(STRPTR)MSG_SCRERR_NOMEM_STR},
    {MSG_SCRERR_NOCHIPMEM,(STRPTR)MSG_SCRERR_NOCHIPMEM_STR},
    {MSG_SCRERR_PUBNOTUNIQUE,(STRPTR)MSG_SCRERR_PUBNOTUNIQUE_STR},
    {MSG_SCRERR_UNKNOWNMODE,(STRPTR)MSG_SCRERR_UNKNOWNMODE_STR},
    {MSG_SCRERR_DEFAULT,(STRPTR)MSG_SCRERR_DEFAULT_STR},
    {MSG_CANT_OPEN_SCR_NAME,(STRPTR)MSG_CANT_OPEN_SCR_NAME_STR},
    {MSG_CANT_FIND_SCR_USE_WB,(STRPTR)MSG_CANT_FIND_SCR_USE_WB_STR},
    {MSG_CANT_LOCK_PBSCR,(STRPTR)MSG_CANT_LOCK_PBSCR_STR},
    {MSG_CANT_GET_VI,(STRPTR)MSG_CANT_GET_VI_STR},
    {MSG_CANT_GET_DI,(STRPTR)MSG_CANT_GET_DI_STR},
    {MSG_CANT_OPEN_WIN,(STRPTR)MSG_CANT_OPEN_WIN_STR},
    {MSG_SET_MARGIN,(STRPTR)MSG_SET_MARGIN_STR},
    {MSG_SEARCH_STRING_NOT_FOUND,(STRPTR)MSG_SEARCH_STRING_NOT_FOUND_STR},
    {MSG_SEARCH_STRING_CANCELED,(STRPTR)MSG_SEARCH_STRING_CANCELED_STR},
    {MSG_SHOWDVI_MESSAGE,(STRPTR)MSG_SHOWDVI_MESSAGE_STR},
    {MSG_ABORT_PRINT_PAGE,(STRPTR)MSG_ABORT_PRINT_PAGE_STR},
    {MSG_OK_CANCEL_REQSTRING,(STRPTR)MSG_OK_CANCEL_REQSTRING_STR},
    {MSG_ABORT_PRINT,(STRPTR)MSG_ABORT_PRINT_STR},
    {MSG_PRINT_IS_ABORTED,(STRPTR)MSG_PRINT_IS_ABORTED_STR},
    {MSG_PRINT_CUR_PAGE,(STRPTR)MSG_PRINT_CUR_PAGE_STR},
    {MSG_WIN_HEADER_FILE,(STRPTR)MSG_WIN_HEADER_FILE_STR},
    {MSG_WIN_HEADER_NO_FILE,(STRPTR)MSG_WIN_HEADER_NO_FILE_STR},
    {MSG_UNAVAILABLE_MODE,(STRPTR)MSG_UNAVAILABLE_MODE_STR},
    {MSG_UNKNOWN_MODE_NAME,(STRPTR)MSG_UNKNOWN_MODE_NAME_STR},
    {MSG_INTERNAL_ERROR,(STRPTR)MSG_INTERNAL_ERROR_STR},
    {MSG_NO_CHIPMEM,(STRPTR)MSG_NO_CHIPMEM_STR},
    {MSG_BUILD_FULL_PAGE,(STRPTR)MSG_BUILD_FULL_PAGE_STR},
    {MSG_NO_MEM,(STRPTR)MSG_NO_MEM_STR},
    {MSG_CANT_SAVE_TO_CLIP,(STRPTR)MSG_CANT_SAVE_TO_CLIP_STR},
    {MSG_CANT_SAVE_TO_IFF,(STRPTR)MSG_CANT_SAVE_TO_IFF_STR},
    {MSG_NO_SIGNAL,(STRPTR)MSG_NO_SIGNAL_STR},
    {MSG_CANT_NOTIFY,(STRPTR)MSG_CANT_NOTIFY_STR},
    {MSG_MARGIN_SET,(STRPTR)MSG_MARGIN_SET_STR},
    {MSG_SHOWDVI_ALREADY_ACTIVE,(STRPTR)MSG_SHOWDVI_ALREADY_ACTIVE_STR},
    {MSG_CANT_ACCESS_FILE,(STRPTR)MSG_CANT_ACCESS_FILE_STR},
    {MSG_AREXX_COMM_START_FAILED,(STRPTR)MSG_AREXX_COMM_START_FAILED_STR},
    {MSG_AREXX_COMM_FAILED,(STRPTR)MSG_AREXX_COMM_FAILED_STR},
    {MSG_DIV_ZERO,(STRPTR)MSG_DIV_ZERO_STR},
    {MSG_CANT_SET_VARIABLE,(STRPTR)MSG_CANT_SET_VARIABLE_STR},
    {MSG_ERROR_IN_CONFIG_FILE,(STRPTR)MSG_ERROR_IN_CONFIG_FILE_STR},
    {MSG_BETA_VERSION,(STRPTR)MSG_BETA_VERSION_STR},
    {MSG_INTERNAL_ERROR_W_ARG,(STRPTR)MSG_INTERNAL_ERROR_W_ARG_STR},
    {MSG_WRONG_MAP_WIDTH,(STRPTR)MSG_WRONG_MAP_WIDTH_STR},
    {MSG_NO_DVI_FILE,(STRPTR)MSG_NO_DVI_FILE_STR},
    {MSG_DVI_FILE_LOST,(STRPTR)MSG_DVI_FILE_LOST_STR},
    {MSG_UNKNOWN_POP_SUBMENU,(STRPTR)MSG_UNKNOWN_POP_SUBMENU_STR},
    {MSG_FILENAME,(STRPTR)MSG_FILENAME_STR},
    {MSG_ENTER_COMMAND,(STRPTR)MSG_ENTER_COMMAND_STR},
    {MSG_EXECUTE_COMMAND,(STRPTR)MSG_EXECUTE_COMMAND_STR},
    {MSG_CANT_EXECUTE_COMMAND,(STRPTR)MSG_CANT_EXECUTE_COMMAND_STR},
    {MSG_USE_WHICH_PUBSCR,(STRPTR)MSG_USE_WHICH_PUBSCR_STR},
    {MSG_DONT_FIND_PUBSCR_USE_WB,(STRPTR)MSG_DONT_FIND_PUBSCR_USE_WB_STR},
    {MSG_SHOWDVI_TEX_SHELL,(STRPTR)MSG_SHOWDVI_TEX_SHELL_STR},
    {MSG_CANT_START,(STRPTR)MSG_CANT_START_STR},
    {MSG_START_SPECIALHOST,(STRPTR)MSG_START_SPECIALHOST_STR},
    {MSG_UNKNOWN_SUBITEM,(STRPTR)MSG_UNKNOWN_SUBITEM_STR},
    {MSG_UNKNOWN_SUBMENU,(STRPTR)MSG_UNKNOWN_SUBMENU_STR},
    {MSG_UNKNOWN_MENU,(STRPTR)MSG_UNKNOWN_MENU_STR},
    {MSG_CANT_OPEN_HELP_FILE,(STRPTR)MSG_CANT_OPEN_HELP_FILE_STR},
    {MSG_SHOWDVI_MENU_HELP,(STRPTR)MSG_SHOWDVI_MENU_HELP_STR},
    {MSG_NO_HELP_FOR_MENU,(STRPTR)MSG_NO_HELP_FOR_MENU_STR},
    {MSG_INTERNAL_ERROR_MENU_ENTRY,(STRPTR)MSG_INTERNAL_ERROR_MENU_ENTRY_STR},
    {MSG_CONFUSED_ABOUT_REQ,(STRPTR)MSG_CONFUSED_ABOUT_REQ_STR},
    {MSG_SHOWDVI_ABOUT_WINDOW,(STRPTR)MSG_SHOWDVI_ABOUT_WINDOW_STR},
    {MSG_CANT_OPEN_ABOUT_REQ,(STRPTR)MSG_CANT_OPEN_ABOUT_REQ_STR},
    {MSG_NEED_REQ_LIB,(STRPTR)MSG_NEED_REQ_LIB_STR},
    {MSG_SHOWDVI_REQUEST,(STRPTR)MSG_SHOWDVI_REQUEST_STR},
    {MSG_OK,(STRPTR)MSG_OK_STR},
    {MSG_CANCEL,(STRPTR)MSG_CANCEL_STR},
    {MSG_SHOWDVI_FATAL_MESSAGE,(STRPTR)MSG_SHOWDVI_FATAL_MESSAGE_STR},
    {MSG_FATAL_WRONG,(STRPTR)MSG_FATAL_WRONG_STR},
    {MSG_FATAL_CHIPMEM,(STRPTR)MSG_FATAL_CHIPMEM_STR},
    {MSG_FATAL_MEMORY,(STRPTR)MSG_FATAL_MEMORY_STR},
    {MSG_FATAL_INTERNAL,(STRPTR)MSG_FATAL_INTERNAL_STR},
    {MSG_FATAL_FATAL,(STRPTR)MSG_FATAL_FATAL_STR},
    {MSG_REALLY_QUIT,(STRPTR)MSG_REALLY_QUIT_STR},
    {MSG_YES_NO,(STRPTR)MSG_YES_NO_STR},
    {MSG_FILE_ISNT_DVI_FILE,(STRPTR)MSG_FILE_ISNT_DVI_FILE_STR},
    {MSG_CANT_ALLOC_FREQ,(STRPTR)MSG_CANT_ALLOC_FREQ_STR},
    {MSG_LOAD_NEW_DVI,(STRPTR)MSG_LOAD_NEW_DVI_STR},
    {MSG_CANT_FOUND_FILE,(STRPTR)MSG_CANT_FOUND_FILE_STR},
    {MSG_CANT_ALLOC_ASLREQ,(STRPTR)MSG_CANT_ALLOC_ASLREQ_STR},
    {MSG_FORMAT_FILE_NAME,(STRPTR)MSG_FORMAT_FILE_NAME_STR},
    {MSG_NOT_WHILE_PRINTING,(STRPTR)MSG_NOT_WHILE_PRINTING_STR},
    {MSG_NO_APP_ICON,(STRPTR)MSG_NO_APP_ICON_STR},
    {MSG_ENTER_A_STRING,(STRPTR)MSG_ENTER_A_STRING_STR},
    {MSG_ENTER_A_NUMBER,(STRPTR)MSG_ENTER_A_NUMBER_STR},
    {MSG_CANT_OPEN_CONFIG,(STRPTR)MSG_CANT_OPEN_CONFIG_STR},
    {MSG_LOAD_CONFIG,(STRPTR)MSG_LOAD_CONFIG_STR},
    {MSG_WRITE_CONFIG,(STRPTR)MSG_WRITE_CONFIG_STR},
    {MSG_NO_AREXX_FOR_FX,(STRPTR)MSG_NO_AREXX_FOR_FX_STR},
    {MSG_NO_ON_OFF,(STRPTR)MSG_NO_ON_OFF_STR},
    {MSG_WRONG_ON_OFF,(STRPTR)MSG_WRONG_ON_OFF_STR},
    {MSG_UNKNOWN_KEYWORD,(STRPTR)MSG_UNKNOWN_KEYWORD_STR},
    {MSG_UNKNOWN_MODEID,(STRPTR)MSG_UNKNOWN_MODEID_STR},
    {MSG_ILLEGAL_COLOR,(STRPTR)MSG_ILLEGAL_COLOR_STR},
    {MSG_WRONG_COLOR_PARAMS,(STRPTR)MSG_WRONG_COLOR_PARAMS_STR},
    {MSG_WRONG_DEF_RESO,(STRPTR)MSG_WRONG_DEF_RESO_STR},
    {MSG_WRONG_VAL_IN_RES_MENU,(STRPTR)MSG_WRONG_VAL_IN_RES_MENU_STR},
    {MSG_WRONG_UNITS,(STRPTR)MSG_WRONG_UNITS_STR},
    {MSG_WRONG_APPICON_POS,(STRPTR)MSG_WRONG_APPICON_POS_STR},
    {MSG_WRONG_MIN_SCREEN_SIZE,(STRPTR)MSG_WRONG_MIN_SCREEN_SIZE_STR},
    {MSG_WRONG_SCREEN_SIZE,(STRPTR)MSG_WRONG_SCREEN_SIZE_STR},
    {MSG_WRONG_MONITOR_SIZE,(STRPTR)MSG_WRONG_MONITOR_SIZE_STR},
    {MSG_WRONG_WIN_POS,(STRPTR)MSG_WRONG_WIN_POS_STR},
    {MSG_WRONG_WIN_SIZE,(STRPTR)MSG_WRONG_WIN_SIZE_STR},
    {MSG_WRONG_MAX_DVIBUF_SIZE,(STRPTR)MSG_WRONG_MAX_DVIBUF_SIZE_STR},
    {MSG_CONFIG_HEADER,(STRPTR)MSG_CONFIG_HEADER_STR},
    {MSG_CONFIG_RGB_BACK,(STRPTR)MSG_CONFIG_RGB_BACK_STR},
    {MSG_CONFIG_RGB_FORE,(STRPTR)MSG_CONFIG_RGB_FORE_STR},
    {MSG_CONFIG_RGB_2,(STRPTR)MSG_CONFIG_RGB_2_STR},
    {MSG_CONFIG_RGB_3,(STRPTR)MSG_CONFIG_RGB_3_STR},
    {MSG_CONFIG_SCROLL_STATE,(STRPTR)MSG_CONFIG_SCROLL_STATE_STR},
    {MSG_CONFIG_BORDER_LINE,(STRPTR)MSG_CONFIG_BORDER_LINE_STR},
    {MSG_CONFIG_LACE,(STRPTR)MSG_CONFIG_LACE_STR},
    {MSG_CONFIG_BEEP,(STRPTR)MSG_CONFIG_BEEP_STR},
    {MSG_CONFIG_ESC,(STRPTR)MSG_CONFIG_ESC_STR},
    {MSG_CONFIG_QUICK_EXIT,(STRPTR)MSG_CONFIG_QUICK_EXIT_STR},
    {MSG_CONFIG_POPUP,(STRPTR)MSG_CONFIG_POPUP_STR},
    {MSG_CONFIG_INT_POPUP,(STRPTR)MSG_CONFIG_INT_POPUP_STR},
    {MSG_CONFIG_BIG_POPUP,(STRPTR)MSG_CONFIG_BIG_POPUP_STR},
    {MSG_CONFIG_MIDDLE_POPUP,(STRPTR)MSG_CONFIG_MIDDLE_POPUP_STR},
    {MSG_CONFIG_USEPHY,(STRPTR)MSG_CONFIG_USEPHY_STR},
    {MSG_CONFIG_ACT_LOAD_AGAIN,(STRPTR)MSG_CONFIG_ACT_LOAD_AGAIN_STR},
    {MSG_CONFIG_USE_OWN_SCR,(STRPTR)MSG_CONFIG_USE_OWN_SCR_STR},
    {MSG_CONFIG_SHOW_PUBSCR_NAME,(STRPTR)MSG_CONFIG_SHOW_PUBSCR_NAME_STR},
    {MSG_CONFIG_DEF_PUBSCR_NAME,(STRPTR)MSG_CONFIG_DEF_PUBSCR_NAME_STR},
    {MSG_CONFIG_WIN_POS,(STRPTR)MSG_CONFIG_WIN_POS_STR},
    {MSG_CONFIG_WIN_SIZE,(STRPTR)MSG_CONFIG_WIN_SIZE_STR},
    {MSG_CONFIG_SCR_POS,(STRPTR)MSG_CONFIG_SCR_POS_STR},
    {MSG_CONFIG_SHOW_WIN_SIZE,(STRPTR)MSG_CONFIG_SHOW_WIN_SIZE_STR},
    {MSG_CONFIG_4_COL_SCR,(STRPTR)MSG_CONFIG_4_COL_SCR_STR},
    {MSG_CONFIG_BACK_HOOK,(STRPTR)MSG_CONFIG_BACK_HOOK_STR},
    {MSG_CONFIG_USE_WB_COLS,(STRPTR)MSG_CONFIG_USE_WB_COLS_STR},
    {MSG_CONFIG_UNIT,(STRPTR)MSG_CONFIG_UNIT_STR},
    {MSG_CONFIG_APP_ICON,(STRPTR)MSG_CONFIG_APP_ICON_STR},
    {MSG_CONFIG_INFO_APP_ICON,(STRPTR)MSG_CONFIG_INFO_APP_ICON_STR},
    {MSG_CONFIG_APP_ICON_POS,(STRPTR)MSG_CONFIG_APP_ICON_POS_STR},
    {MSG_CONIFG_SCRIPT_FILE,(STRPTR)MSG_CONIFG_SCRIPT_FILE_STR},
    {MSG_CONFIG_TEX_SERVER,(STRPTR)MSG_CONFIG_TEX_SERVER_STR},
    {MSG_CONFIG_SCR_SIZE,(STRPTR)MSG_CONFIG_SCR_SIZE_STR},
    {MSG_CONFIG_SCR_MODE,(STRPTR)MSG_CONFIG_SCR_MODE_STR},
    {MSG_CONFIG_MONITOR_SIZE,(STRPTR)MSG_CONFIG_MONITOR_SIZE_STR},
    {MSG_CONFIG_ALWBMFAST,(STRPTR)MSG_CONFIG_ALWBMFAST_STR},
    {MSG_CONFIG_SMARTWIN,(STRPTR)MSG_CONFIG_SMARTWIN_STR},
    {MSG_CONFIG_MAX_DVIBUF,(STRPTR)MSG_CONFIG_MAX_DVIBUF_STR},
    {MSG_CONFIG_DEF_RESO,(STRPTR)MSG_CONFIG_DEF_RESO_STR},
    {MSG_CONFIG_RESO_MENU,(STRPTR)MSG_CONFIG_RESO_MENU_STR},
    {MSG_CONFIG_SAVED_OK,(STRPTR)MSG_CONFIG_SAVED_OK_STR},
    {MSG_CANT_SAVE_CONFIG,(STRPTR)MSG_CANT_SAVE_CONFIG_STR},
    {MSG_CONFIG_APEN,(STRPTR)MSG_CONFIG_APEN_STR},
    {MSG_CONFIG_BPEN,(STRPTR)MSG_CONFIG_BPEN_STR},
    {MSG_WRONG_PEN,(STRPTR)MSG_WRONG_PEN_STR},
    {MSG_MEASURE_WINDOW,(STRPTR)MSG_MEASURE_WINDOW_STR},
    {MSG_CANT_OPEN_MESSWIN,(STRPTR)MSG_CANT_OPEN_MESSWIN_STR},
    {MSG_MESSWIN_WIDTH,(STRPTR)MSG_MESSWIN_WIDTH_STR},
    {MSG_MESSWIN_HEIGHT,(STRPTR)MSG_MESSWIN_HEIGHT_STR},
    {MSG_MESSWIN_DEL_X,(STRPTR)MSG_MESSWIN_DEL_X_STR},
    {MSG_MESSWIN_X,(STRPTR)MSG_MESSWIN_X_STR},
    {MSG_MESSWIN_DEL_Y,(STRPTR)MSG_MESSWIN_DEL_Y_STR},
    {MSG_MESSWIN_Y,(STRPTR)MSG_MESSWIN_Y_STR},
    {MSG_MESSWIN_MAG,(STRPTR)MSG_MESSWIN_MAG_STR},
    {MSG_AREXX_REPLY_CODE,(STRPTR)MSG_AREXX_REPLY_CODE_STR},
    {MSG_EXECUTE_TEX_SCRIPT,(STRPTR)MSG_EXECUTE_TEX_SCRIPT_STR},
    {MSG_NEWCLI_TITLE,(STRPTR)MSG_NEWCLI_TITLE_STR},
    {MSG_PREFWIN_GADARR_0,(STRPTR)MSG_PREFWIN_GADARR_0_STR},
    {MSG_PREFWIN_GADARR_1,(STRPTR)MSG_PREFWIN_GADARR_1_STR},
    {MSG_PREFWIN_GADARR_2,(STRPTR)MSG_PREFWIN_GADARR_2_STR},
    {MSG_PREFWIN_GADARR_3,(STRPTR)MSG_PREFWIN_GADARR_3_STR},
    {MSG_PREFWIN_GADARR_4,(STRPTR)MSG_PREFWIN_GADARR_4_STR},
    {MSG_PREFWIN_GADARR_5,(STRPTR)MSG_PREFWIN_GADARR_5_STR},
    {MSG_PREFWIN_GADARR_6,(STRPTR)MSG_PREFWIN_GADARR_6_STR},
    {MSG_PREFWIN_GADARR_7,(STRPTR)MSG_PREFWIN_GADARR_7_STR},
    {MSG_PREFWIN_GADARR_8,(STRPTR)MSG_PREFWIN_GADARR_8_STR},
    {MSG_PREFWIN_GADARR_9,(STRPTR)MSG_PREFWIN_GADARR_9_STR},
    {MSG_PREFWIN_GADARR_10,(STRPTR)MSG_PREFWIN_GADARR_10_STR},
    {MSG_PREFWIN_GADTOOLS,(STRPTR)MSG_PREFWIN_GADTOOLS_STR},
    {MSG_PREFWIN_WIN_TITLE,(STRPTR)MSG_PREFWIN_WIN_TITLE_STR},
    {MSG_PREFWIN_CANT_OPEN,(STRPTR)MSG_PREFWIN_CANT_OPEN_STR},
    {MSG_SEARCHWIN_WIN_TITLE,(STRPTR)MSG_SEARCHWIN_WIN_TITLE_STR},
    {MSG_SEARCHWIN_DOSEARCH,(STRPTR)MSG_SEARCHWIN_DOSEARCH_STR},
    {MSG_SEARCHWIN_FOUND,(STRPTR)MSG_SEARCHWIN_FOUND_STR},
    {MSG_SEARCHWIN_CANT_OPEN,(STRPTR)MSG_SEARCHWIN_CANT_OPEN_STR},
    {MSG_SEARCHWIN_STRING,(STRPTR)MSG_SEARCHWIN_STRING_STR},
    {MSG_SEARCHWIN_SEARCH,(STRPTR)MSG_SEARCHWIN_SEARCH_STR},
    {MSG_SEARCHWIN_CANCEL,(STRPTR)MSG_SEARCHWIN_CANCEL_STR},
    {MSG_NO_FILE_GIVEN,(STRPTR)MSG_NO_FILE_GIVEN_STR},
    {MSG_BUILD_BITMAP,(STRPTR)MSG_BUILD_BITMAP_STR},
    {MSG_STACK_OVER,(STRPTR)MSG_STACK_OVER_STR},
    {MSG_STACK_UNDER,(STRPTR)MSG_STACK_UNDER_STR},
    {MSG_PRE_IN_FILE,(STRPTR)MSG_PRE_IN_FILE_STR},
    {MSG_POST_IN_FILE,(STRPTR)MSG_POST_IN_FILE_STR},
    {MSG_POST_POST,(STRPTR)MSG_POST_POST_STR},
    {MSG_DVI_FILE_ERROR,(STRPTR)MSG_DVI_FILE_ERROR_STR},
    {MSG_ALREADY_FIRST,(STRPTR)MSG_ALREADY_FIRST_STR},
    {MSG_ALREADY_LAST,(STRPTR)MSG_ALREADY_LAST_STR},
    {MSG_PAGE_NOT_FOUND,(STRPTR)MSG_PAGE_NOT_FOUND_STR},
    {MSG_MISSING_PRE,(STRPTR)MSG_MISSING_PRE_STR},
    {MSG_WRONG_DVI_TYPE,(STRPTR)MSG_WRONG_DVI_TYPE_STR},
    {MSG_LOADING_DVI,(STRPTR)MSG_LOADING_DVI_STR},
    {MSG_INCOMPLETE_DVI_PRELOAD,(STRPTR)MSG_INCOMPLETE_DVI_PRELOAD_STR},
    {MSG_SCAN_DVI_FILE,(STRPTR)MSG_SCAN_DVI_FILE_STR},
    {MSG_HELP_HELP,(STRPTR)MSG_HELP_HELP_STR},
    {MSG_HELP_FONTDIR,(STRPTR)MSG_HELP_FONTDIR_STR},
    {MSG_HELP_FONTMEM,(STRPTR)MSG_HELP_FONTMEM_STR},
    {MSG_HELP_FROM,(STRPTR)MSG_HELP_FROM_STR},
    {MSG_HELP_HOFFSET,(STRPTR)MSG_HELP_HOFFSET_STR},
    {MSG_HELP_HOFFSET_2,(STRPTR)MSG_HELP_HOFFSET_2_STR},
    {MSG_HELP_VOFFSET,(STRPTR)MSG_HELP_VOFFSET_STR},
    {MSG_HELP_VOFFSET_2,(STRPTR)MSG_HELP_VOFFSET_2_STR},
    {MSG_HELP_PRELOAD,(STRPTR)MSG_HELP_PRELOAD_STR},
    {MSG_HELP_RESOLUTION,(STRPTR)MSG_HELP_RESOLUTION_STR},
    {MSG_HELP_STATISTIC,(STRPTR)MSG_HELP_STATISTIC_STR},
    {MSG_HELP_DEBUGSTAT,(STRPTR)MSG_HELP_DEBUGSTAT_STR},
    {MSG_HELP_LOGNAME,(STRPTR)MSG_HELP_LOGNAME_STR},
    {MSG_HELP_NOLOG,(STRPTR)MSG_HELP_NOLOG_STR},
    {MSG_HELP_PRIORITY,(STRPTR)MSG_HELP_PRIORITY_STR},
    {MSG_HELP_PRINTAUTHOR,(STRPTR)MSG_HELP_PRINTAUTHOR_STR},
    {MSG_HELP_DVIFILE,(STRPTR)MSG_HELP_DVIFILE_STR},
    {MSG_COPYRIGHT,(STRPTR)MSG_COPYRIGHT_STR},
    {MSG_USAGE,(STRPTR)MSG_USAGE_STR},
    {MSG_CANT_FIND_FILE,(STRPTR)MSG_CANT_FIND_FILE_STR},
    {MSG_NOT_ENOUGH_MEM_BYTES,(STRPTR)MSG_NOT_ENOUGH_MEM_BYTES_STR},
    {MSG_PROGRAM_END_ERR,(STRPTR)MSG_PROGRAM_END_ERR_STR},
    {MSG_USER_ABORT,(STRPTR)MSG_USER_ABORT_STR},
    {MSG_PROGRAM_END_OK,(STRPTR)MSG_PROGRAM_END_OK_STR},
    {MSG_DVIP_PRINT_FINISHED,(STRPTR)MSG_DVIP_PRINT_FINISHED_STR},
    {MSG_FATAL,(STRPTR)MSG_FATAL_STR},
    {MSG_LOG_FILE_CREATED,(STRPTR)MSG_LOG_FILE_CREATED_STR},
    {MSG_LINE_BUFFER_OVERFLOW,(STRPTR)MSG_LINE_BUFFER_OVERFLOW_STR},
    {MSG_BREAK,(STRPTR)MSG_BREAK_STR},
    {MSG_BREAK_IO,(STRPTR)MSG_BREAK_IO_STR},
    {MSG_CALL_MF,(STRPTR)MSG_CALL_MF_STR},
    {MSG_CANT_OPEN,(STRPTR)MSG_CANT_OPEN_STR},
    {MSG_DVIP_WHOLE_BITMAP_IN_RAM,(STRPTR)MSG_DVIP_WHOLE_BITMAP_IN_RAM_STR},
    {MSG_CANT_ALLOC_BITMAP,(STRPTR)MSG_CANT_ALLOC_BITMAP_STR},
    {MSG_DVIP_PARTS,(STRPTR)MSG_DVIP_PARTS_STR},
    {MSG_DVIP_SAVE_CLIP,(STRPTR)MSG_DVIP_SAVE_CLIP_STR},
    {MSG_DVIP_SAVE_IFF,(STRPTR)MSG_DVIP_SAVE_IFF_STR},
    {MSG_DVIP_CANT_SAVE_IFF,(STRPTR)MSG_DVIP_CANT_SAVE_IFF_STR},
    {MSG_SPECIAL_TOO_LONG,(STRPTR)MSG_SPECIAL_TOO_LONG_STR},
    {MSG_NO_MEM_FOR_SPECIAL,(STRPTR)MSG_NO_MEM_FOR_SPECIAL_STR},
    {MSG_PICT_OUT_LEFT,(STRPTR)MSG_PICT_OUT_LEFT_STR},
    {MSG_PICT_OUT_RIGHT,(STRPTR)MSG_PICT_OUT_RIGHT_STR},
    {MSG_NO_MEM_FOR_SPECIAL_BITMAP,(STRPTR)MSG_NO_MEM_FOR_SPECIAL_BITMAP_STR},
    {MSG_UNKNOWN_PICT_LOC,(STRPTR)MSG_UNKNOWN_PICT_LOC_STR},
    {MSG_ILLEG_PS_COMM,(STRPTR)MSG_ILLEG_PS_COMM_STR},
    {MSG_TOO_MANY_POINTS,(STRPTR)MSG_TOO_MANY_POINTS_STR},
    {MSG_MALFORMED_PATH_COMM,(STRPTR)MSG_MALFORMED_PATH_COMM_STR},
    {MSG_BAD_DVI_FILE_END,(STRPTR)MSG_BAD_DVI_FILE_END_STR},
    {MSG_BAD_FONT_DEFS,(STRPTR)MSG_BAD_FONT_DEFS_STR},
    {MSG_MISS_POST_POST,(STRPTR)MSG_MISS_POST_POST_STR},
    {MSG_MISS_POST,(STRPTR)MSG_MISS_POST_STR},
    {MSG_LOG_PAGE_DIM,(STRPTR)MSG_LOG_PAGE_DIM_STR},
    {MSG_LOG_HORIZ,(STRPTR)MSG_LOG_HORIZ_STR},
    {MSG_LOG_VERT,(STRPTR)MSG_LOG_VERT_STR},
    {MSG_LOG_MAG,(STRPTR)MSG_LOG_MAG_STR},
    {MSG_LOG_OFFSET,(STRPTR)MSG_LOG_OFFSET_STR},
    {MSG_LOG_THOFF,(STRPTR)MSG_LOG_THOFF_STR},
    {MSG_LOG_HOFF,(STRPTR)MSG_LOG_HOFF_STR},
    {MSG_LOG_TVOFF,(STRPTR)MSG_LOG_TVOFF_STR},
    {MSG_LOG_VOFF,(STRPTR)MSG_LOG_VOFF_STR},
    {MSG_LOG_RESO,(STRPTR)MSG_LOG_RESO_STR},
    {MSG_DVI_STACK_TOO_SMALL,(STRPTR)MSG_DVI_STACK_TOO_SMALL_STR},
    {MSG_LOG_NUM_PAGES,(STRPTR)MSG_LOG_NUM_PAGES_STR},
    {MSG_CANT_OPEN_FONTLIB,(STRPTR)MSG_CANT_OPEN_FONTLIB_STR},
    {MSG_NOT_A_FONTLIB,(STRPTR)MSG_NOT_A_FONTLIB_STR},
    {MSG_OLD_FONTLIB,(STRPTR)MSG_OLD_FONTLIB_STR},
    {MSG_TOO_MANY_FONTLIB_LEVELS,(STRPTR)MSG_TOO_MANY_FONTLIB_LEVELS_STR},
    {MSG_ERROR_READING_FONTLIB_DIR,(STRPTR)MSG_ERROR_READING_FONTLIB_DIR_STR},
    {MSG_BAD_PK_FILE,(STRPTR)MSG_BAD_PK_FILE_STR},
    {MSG_UNEXPECTED_EOF_IN_PK,(STRPTR)MSG_UNEXPECTED_EOF_IN_PK_STR},
    {MSG_EXPECTED_PRE_IN_PK,(STRPTR)MSG_EXPECTED_PRE_IN_PK_STR},
    {MSG_WRONG_ID_IN_PK,(STRPTR)MSG_WRONG_ID_IN_PK_STR},
    {MSG_CHECKSUM_MISS_IN_PK,(STRPTR)MSG_CHECKSUM_MISS_IN_PK_STR},
    {MSG_CHAR_CODE_OUT_OF_RANGE_IN_PK,(STRPTR)MSG_CHAR_CODE_OUT_OF_RANGE_IN_PK_STR},
    {MSG_PACKET_LENGTH_SMALL_IN_PK,(STRPTR)MSG_PACKET_LENGTH_SMALL_IN_PK_STR},
    {MSG_NO_MEM_FOR_CHAR,(STRPTR)MSG_NO_MEM_FOR_CHAR_STR},
    {MSG_UNEXPECTED_COMM_IN_PK,(STRPTR)MSG_UNEXPECTED_COMM_IN_PK_STR},
    {MSG_RELEASE_CHARS,(STRPTR)MSG_RELEASE_CHARS_STR},
    {MSG_INTERNAL_ERROR_SPRINTF,(STRPTR)MSG_INTERNAL_ERROR_SPRINTF_STR},
    {MSG_UNKNOWN_FORMAT_SPRINTF,(STRPTR)MSG_UNKNOWN_FORMAT_SPRINTF_STR},
    {MSG_STRING_OVERFLOW_SPRINTF,(STRPTR)MSG_STRING_OVERFLOW_SPRINTF_STR},
    {MSG_ERROR_SEARCH_LIST_LIB,(STRPTR)MSG_ERROR_SEARCH_LIST_LIB_STR},
    {MSG_ERROR_SEARCH_LIST_PK,(STRPTR)MSG_ERROR_SEARCH_LIST_PK_STR},
    {MSG_FONT_UNDEFINED,(STRPTR)MSG_FONT_UNDEFINED_STR},
    {MSG_FONT_ALREADY_DEFINED,(STRPTR)MSG_FONT_ALREADY_DEFINED_STR},
    {MSG_FONT_STR_UNDEFINED,(STRPTR)MSG_FONT_STR_UNDEFINED_STR},
    {MSG_RELOAD_FONT,(STRPTR)MSG_RELOAD_FONT_STR},
    {MSG_FONTMEM_USED,(STRPTR)MSG_FONTMEM_USED_STR},
    {MSG_LOAD_LOADED_FONT,(STRPTR)MSG_LOAD_LOADED_FONT_STR},
    {MSG_LOAD_FONT,(STRPTR)MSG_LOAD_FONT_STR},
    {MSG_INTERNAL_ERROR_LIB_MISMATCH,(STRPTR)MSG_INTERNAL_ERROR_LIB_MISMATCH_STR},
    {MSG_LOAD_FONT_MEM_USED,(STRPTR)MSG_LOAD_FONT_MEM_USED_STR},
    {MSG_FOUND_IN_LIB,(STRPTR)MSG_FOUND_IN_LIB_STR},
    {MSG_FOUND_AS_PK,(STRPTR)MSG_FOUND_AS_PK_STR},
    {MSG_MEMORY_ERROR,(STRPTR)MSG_MEMORY_ERROR_STR},
    {MSG_SUBSTITUTE_FONT,(STRPTR)MSG_SUBSTITUTE_FONT_STR},
    {MSG_FONT_NOT_FOUND,(STRPTR)MSG_FONT_NOT_FOUND_STR},
    {MSG_FONT_XY_NOT_FOUND,(STRPTR)MSG_FONT_XY_NOT_FOUND_STR},
    {MSG_TRY_SUBSTITUTE,(STRPTR)MSG_TRY_SUBSTITUTE_STR},
    {MSG_PREDEF_FONT_IN_LIB,(STRPTR)MSG_PREDEF_FONT_IN_LIB_STR},
    {MSG_PREDEF_FONT_NOT_FOUND,(STRPTR)MSG_PREDEF_FONT_NOT_FOUND_STR},
    {MSG_INTERNAL_ERROR_FMT_STR,(STRPTR)MSG_INTERNAL_ERROR_FMT_STR_STR},
    {MSG_BYTES_USED_FOR_CHARS,(STRPTR)MSG_BYTES_USED_FOR_CHARS_STR},
    {MSG_SEARCHPATH_LIB_ENTRIES_NR,(STRPTR)MSG_SEARCHPATH_LIB_ENTRIES_NR_STR},
    {MSG_SEARCHPATH_PK_ENTRIES_NR,(STRPTR)MSG_SEARCHPATH_PK_ENTRIES_NR_STR},
    {MSG_LOG_FONTDEF_START,(STRPTR)MSG_LOG_FONTDEF_START_STR},
    {MSG_LOG_FONTDEF_LIB_FOUND,(STRPTR)MSG_LOG_FONTDEF_LIB_FOUND_STR},
    {MSG_LOG_FONTDEF_LIB_DEF,(STRPTR)MSG_LOG_FONTDEF_LIB_DEF_STR},
    {MSG_LOG_FONTDEF_LIB_NOT_FOUND,(STRPTR)MSG_LOG_FONTDEF_LIB_NOT_FOUND_STR},
    {MSG_LOG_FONTDEF_PK_FOUND,(STRPTR)MSG_LOG_FONTDEF_PK_FOUND_STR},
    {MSG_LOG_FONTDEF_PK_PREDEF,(STRPTR)MSG_LOG_FONTDEF_PK_PREDEF_STR},
    {MSG_LOG_FONTDEF_PK_PREDEF_NOT_FOUND,(STRPTR)MSG_LOG_FONTDEF_PK_PREDEF_NOT_FOUND_STR},
    {MSG_LOG_FONTDEF_NR_BASE_PK_DIRS_PREDEF,(STRPTR)MSG_LOG_FONTDEF_NR_BASE_PK_DIRS_PREDEF_STR},
    {MSG_LOG_FONTDEF_NO_PK_DIRS_PREDEF,(STRPTR)MSG_LOG_FONTDEF_NO_PK_DIRS_PREDEF_STR},
    {MSG_LOG_FONTDEF_END,(STRPTR)MSG_LOG_FONTDEF_END_STR},
    {MSG_CANT_OPEN_FONT_CONFIG,(STRPTR)MSG_CANT_OPEN_FONT_CONFIG_STR},
    {MSG_LOAD_FONT_CONFIG,(STRPTR)MSG_LOAD_FONT_CONFIG_STR},
    {MSG_CANT_PARSE_FONT_CONFIG_LINE,(STRPTR)MSG_CANT_PARSE_FONT_CONFIG_LINE_STR},
    {MSG_TOO_FEW_ARGS,(STRPTR)MSG_TOO_FEW_ARGS_STR},
    {MSG_NOT_A_VALID_INT,(STRPTR)MSG_NOT_A_VALID_INT_STR},
    {MSG_CANT_READ_DPI_FONT_COMM,(STRPTR)MSG_CANT_READ_DPI_FONT_COMM_STR},
    {MSG_UNKNOWN_KEYWORD_IN_CONFIG,(STRPTR)MSG_UNKNOWN_KEYWORD_IN_CONFIG_STR},
    {MSG_FONT_REMOVED,(STRPTR)MSG_FONT_REMOVED_STR},
    {MSG_FONT_REMOVED_USED,(STRPTR)MSG_FONT_REMOVED_USED_STR},
    {MSG_ERROR_IN_FONTVOLS,(STRPTR)MSG_ERROR_IN_FONTVOLS_STR},
    {MSG_FONTVOLS_FIRST_DOT,(STRPTR)MSG_FONTVOLS_FIRST_DOT_STR},
    {MSG_COPY_FONT_TO_CACHE,(STRPTR)MSG_COPY_FONT_TO_CACHE_STR},
    {MSG_CANT_COPY_FONT_TO_CACHE,(STRPTR)MSG_CANT_COPY_FONT_TO_CACHE_STR},
    {MSG_PARSE_BAD_NR_ARGS,(STRPTR)MSG_PARSE_BAD_NR_ARGS_STR},
    {MSG_PARSE_BAD_ON_OFF,(STRPTR)MSG_PARSE_BAD_ON_OFF_STR},
    {MSG_PARSE_CANT_PARSE,(STRPTR)MSG_PARSE_CANT_PARSE_STR},
    {MSG_PARSE_CANT_PARSE_IGNORE,(STRPTR)MSG_PARSE_CANT_PARSE_IGNORE_STR},
    {MSG_PARSE_ILLEG_ARG,(STRPTR)MSG_PARSE_ILLEG_ARG_STR},
    {MSG_UNPACK_RECURSIV,(STRPTR)MSG_UNPACK_RECURSIV_STR},
    {MSG_UNPACK_MORE_BITS,(STRPTR)MSG_UNPACK_MORE_BITS_STR},
    {MSG_PORT_ALREADY_EXISTS,(STRPTR)MSG_PORT_ALREADY_EXISTS_STR},
    {MSG_CANT_OPEN_PRINTER_PORT,(STRPTR)MSG_CANT_OPEN_PRINTER_PORT_STR},
    {MSG_CREATEEXTIO_FAILED,(STRPTR)MSG_CREATEEXTIO_FAILED_STR},
    {MSG_YOU_PRINT_GENERIC,(STRPTR)MSG_YOU_PRINT_GENERIC_STR},
    {MSG_GENERIC_PRINT_NAME,(STRPTR)MSG_GENERIC_PRINT_NAME_STR},
    {MSG_GENERIC_PRINT_RESO,(STRPTR)MSG_GENERIC_PRINT_RESO_STR},
    {MSG_GENERIC_PRINT_MAXD,(STRPTR)MSG_GENERIC_PRINT_MAXD_STR},
    {MSG_PRT_ERROR_NO_ERROR,(STRPTR)MSG_PRT_ERROR_NO_ERROR_STR},
    {MSG_PRT_ERROR_ABORT,(STRPTR)MSG_PRT_ERROR_ABORT_STR},
    {MSG_PRT_ERROR_NO_GFX,(STRPTR)MSG_PRT_ERROR_NO_GFX_STR},
    {MSG_PRT_ERROR_ILLDIM,(STRPTR)MSG_PRT_ERROR_ILLDIM_STR},
    {MSG_PRT_ERROR_NO_MEM_VARS,(STRPTR)MSG_PRT_ERROR_NO_MEM_VARS_STR},
    {MSG_PRT_ERROR_NO_MEM_BUFF,(STRPTR)MSG_PRT_ERROR_NO_MEM_BUFF_STR},
    {MSG_PRT_ERROR_UNKNOWN_ERR,(STRPTR)MSG_PRT_ERROR_UNKNOWN_ERR_STR},
    {MSG_PRT_ERR,(STRPTR)MSG_PRT_ERR_STR},
    {MSG_TRY_CLEAR_PRT,(STRPTR)MSG_TRY_CLEAR_PRT_STR},
    {MSG_CLEAR_FAILED,(STRPTR)MSG_CLEAR_FAILED_STR},
    {MSG_GETOPT_LINEBUF_OVERFLOW,(STRPTR)MSG_GETOPT_LINEBUF_OVERFLOW_STR},
    {MSG_GETOPT_ERROR,(STRPTR)MSG_GETOPT_ERROR_STR},
    {MSG_GETOPT_ERROR_KEY,(STRPTR)MSG_GETOPT_ERROR_KEY_STR},
    {MSG_GETOPT_NO_OPTION_STRING,(STRPTR)MSG_GETOPT_NO_OPTION_STRING_STR},
    {MSG_GETOPT_MISSING_NUM,(STRPTR)MSG_GETOPT_MISSING_NUM_STR},
    {MSG_GETOPT_NO_NUMBER,(STRPTR)MSG_GETOPT_NO_NUMBER_STR},
    {MSG_GETOPT_MISSING_REAL,(STRPTR)MSG_GETOPT_MISSING_REAL_STR},
    {MSG_GETOPT_NO_REAL,(STRPTR)MSG_GETOPT_NO_REAL_STR},
    {MSG_GETOPT_MISSING_TEX,(STRPTR)MSG_GETOPT_MISSING_TEX_STR},
    {MSG_GETOPT_NO_TEX,(STRPTR)MSG_GETOPT_NO_TEX_STR},
    {MSG_GETOPT_UNKNOWN_PARAM,(STRPTR)MSG_GETOPT_UNKNOWN_PARAM_STR},
    {MSG_GETOPT_WRONG_ONOFF_PARAM,(STRPTR)MSG_GETOPT_WRONG_ONOFF_PARAM_STR},
    {MSG_GETOPT_NO_PARAM_EXPECTED,(STRPTR)MSG_GETOPT_NO_PARAM_EXPECTED_STR},
    {MSG_GETOPT_UNKNOWN_KEYWORD,(STRPTR)MSG_GETOPT_UNKNOWN_KEYWORD_STR},
    {MSG_GETOPT_TOO_MANY_ENV_ARGS,(STRPTR)MSG_GETOPT_TOO_MANY_ENV_ARGS_STR},
    {MSG_GETOPT_NO_MEM_FOR_ENV,(STRPTR)MSG_GETOPT_NO_MEM_FOR_ENV_STR},
    {MSG_GETOPT_SUPER_FILE,(STRPTR)MSG_GETOPT_SUPER_FILE_STR},
    {MSG_GETOPT_NO_INFO,(STRPTR)MSG_GETOPT_NO_INFO_STR},
    {MSG_GETOPT_PARAM_REQU,(STRPTR)MSG_GETOPT_PARAM_REQU_STR},
    {MSG_GETOPT_TAB_TAB_DEF,(STRPTR)MSG_GETOPT_TAB_TAB_DEF_STR},
    {MSG_GETOPT_TAB_DEF,(STRPTR)MSG_GETOPT_TAB_DEF_STR},
    {MSG_GETOPT_NO_DEFAULT,(STRPTR)MSG_GETOPT_NO_DEFAULT_STR},
    {MSG_GETOPT_PRESS_RET,(STRPTR)MSG_GETOPT_PRESS_RET_STR},
    {MSG_SPECIAL_WAIT_FOR_PICT,(STRPTR)MSG_SPECIAL_WAIT_FOR_PICT_STR},
    {MSG_SPECIAL_EXPT_REPLY_SPECIAL,(STRPTR)MSG_SPECIAL_EXPT_REPLY_SPECIAL_STR},
    {MSG_SPECIAL_RET_FROM_SPECIAL,(STRPTR)MSG_SPECIAL_RET_FROM_SPECIAL_STR},
    {MSG_SPECIAL_EXPT_REPLY_TPIC,(STRPTR)MSG_SPECIAL_EXPT_REPLY_TPIC_STR},
    {MSG_SPECIAL_EXPT_REPLY_BITMAP,(STRPTR)MSG_SPECIAL_EXPT_REPLY_BITMAP_STR},
    {MSG_SPECIAL_PICT_RECEIVED,(STRPTR)MSG_SPECIAL_PICT_RECEIVED_STR},
    {MSG_SPECIAL_FOUND_NO_SPECIAL,(STRPTR)MSG_SPECIAL_FOUND_NO_SPECIAL_STR},
    {MSG_SPECIAL_CANT_CREATE_PORT,(STRPTR)MSG_SPECIAL_CANT_CREATE_PORT_STR},
    {MSG_NO_DVI_FILENAME,(STRPTR)MSG_NO_DVI_FILENAME_STR},
    {MSG_CANT_OPEN_DVI_FILE,(STRPTR)MSG_CANT_OPEN_DVI_FILE_STR},
    {MSG_INCOMPLETE_DVI_REVERSE,(STRPTR)MSG_INCOMPLETE_DVI_REVERSE_STR},
    {MSG_INCOMPLETE_DVI,(STRPTR)MSG_INCOMPLETE_DVI_STR},
    {MSG_UNDEFINED_DVI_COMMAND,(STRPTR)MSG_UNDEFINED_DVI_COMMAND_STR},
    {MSG_NO_LAND_WITH_GENERIC,(STRPTR)MSG_NO_LAND_WITH_GENERIC_STR},
    {MSG_NO_LAND_XDPI_YDPI,(STRPTR)MSG_NO_LAND_XDPI_YDPI_STR},
    {MSG_PRINTER_NAME,(STRPTR)MSG_PRINTER_NAME_STR},
    {MSG_PRINTER_ID,(STRPTR)MSG_PRINTER_ID_STR},
    {MSG_MISSING_KEYWORD,(STRPTR)MSG_MISSING_KEYWORD_STR},
    {MSG_INCOMPLETE_PRT_DESC,(STRPTR)MSG_INCOMPLETE_PRT_DESC_STR},
    {MSG_USE_DRAFT_OPT,(STRPTR)MSG_USE_DRAFT_OPT_STR},
    {MSG_PRT_RESO,(STRPTR)MSG_PRT_RESO_STR},
    {MSG_PRT_WIDTH,(STRPTR)MSG_PRT_WIDTH_STR},
    {MSG_PRT_PASSES,(STRPTR)MSG_PRT_PASSES_STR},
    {MSG_PRT_BITMAP_HEIGHT,(STRPTR)MSG_PRT_BITMAP_HEIGHT_STR},
    {MSG_PRT_DEF_BUF_SIZE,(STRPTR)MSG_PRT_DEF_BUF_SIZE_STR},
    {MSG_PRT_MOVE_TO_POINT,(STRPTR)MSG_PRT_MOVE_TO_POINT_STR},
    {MSG_PRT_ONE_GRAPHIC_COMMAND,(STRPTR)MSG_PRT_ONE_GRAPHIC_COMMAND_STR},
    {MSG_PRT_SKIP_WITH_SPACES,(STRPTR)MSG_PRT_SKIP_WITH_SPACES_STR},
    {MSG_PRT_BAD_RESO,(STRPTR)MSG_PRT_BAD_RESO_STR},
    {MSG_PRT_USE_RES,(STRPTR)MSG_PRT_USE_RES_STR},
    {MSG_UNKNOWN_BLANKING,(STRPTR)MSG_UNKNOWN_BLANKING_STR},
    {MSG_ILLEG_PARM_FOR_KEY,(STRPTR)MSG_ILLEG_PARM_FOR_KEY_STR},
    {MSG_GROUPING_RANGE,(STRPTR)MSG_GROUPING_RANGE_STR},
    {MSG_CANT_OPEN_PRT_CONFIG,(STRPTR)MSG_CANT_OPEN_PRT_CONFIG_STR},
    {MSG_READ_PRT_CONFIG,(STRPTR)MSG_READ_PRT_CONFIG_STR},
    {MSG_THATS_ALL,(STRPTR)MSG_THATS_ALL_STR},
    {MSG_AVAILABLE_PRINTERS,(STRPTR)MSG_AVAILABLE_PRINTERS_STR},
    {MSG_MAX_GROUPING,(STRPTR)MSG_MAX_GROUPING_STR},
    {MSG_PRINT_WITH,(STRPTR)MSG_PRINT_WITH_STR},
    {MSG_UNKNOWN_PRT_CONFIG,(STRPTR)MSG_UNKNOWN_PRT_CONFIG_STR},
    {MSG_PRT_NO_OUTPUT_REDIRECTION,(STRPTR)MSG_PRT_NO_OUTPUT_REDIRECTION_STR},
    {MSG_CANT_OPEN_OUTPUT_FILE,(STRPTR)MSG_CANT_OPEN_OUTPUT_FILE_STR},
    {MSG_NO_PRT_TYPE_FLAG,(STRPTR)MSG_NO_PRT_TYPE_FLAG_STR},
    {MSG_KEY_TWICE,(STRPTR)MSG_KEY_TWICE_STR},
    {MSG_KEY_CONTEXT,(STRPTR)MSG_KEY_CONTEXT_STR},
    {MSG_CANT_WRITE_OUTPUT_FILE,(STRPTR)MSG_CANT_WRITE_OUTPUT_FILE_STR},
    {MSG_WRONG_NUM_COPIES,(STRPTR)MSG_WRONG_NUM_COPIES_STR},
    {MSG_PRINT_AT_LEAST_ONE_PAGE,(STRPTR)MSG_PRINT_AT_LEAST_ONE_PAGE_STR},
    {MSG_WRONG_RESO_FORMAT,(STRPTR)MSG_WRONG_RESO_FORMAT_STR},
    {MSG_NO_ODD_NO_EVEN,(STRPTR)MSG_NO_ODD_NO_EVEN_STR},
    {MSG_DENSITY_RANGE,(STRPTR)MSG_DENSITY_RANGE_STR},
    {MSG_BITMAP_MEM_MINIMUM,(STRPTR)MSG_BITMAP_MEM_MINIMUM_STR},
    {MSG_TRY_HELP_SHOWP,(STRPTR)MSG_TRY_HELP_SHOWP_STR},
    {MSG_UNKNOWN_PAPER_SIZE,(STRPTR)MSG_UNKNOWN_PAPER_SIZE_STR},
    {MSG_REVERSE_TWOUP_ERR,(STRPTR)MSG_REVERSE_TWOUP_ERR_STR},
    {MSG_PAPER_WIDTH_HEIGHT_ERR,(STRPTR)MSG_PAPER_WIDTH_HEIGHT_ERR_STR},
    {MSG_OPTIONS_HELP,(STRPTR)MSG_OPTIONS_HELP_STR},
    {MSG_OPTIONS_GUI,(STRPTR)MSG_OPTIONS_GUI_STR},
    {MSG_OPTIONS_FONTDIR,(STRPTR)MSG_OPTIONS_FONTDIR_STR},
    {MSG_OPTIONS_FONTMEM,(STRPTR)MSG_OPTIONS_FONTMEM_STR},
    {MSG_OPTIONS_MAXBITMEM,(STRPTR)MSG_OPTIONS_MAXBITMEM_STR},
    {MSG_OPTIONS_MAXBITMEM2,(STRPTR)MSG_OPTIONS_MAXBITMEM2_STR},
    {MSG_OPTIONS_PRTBUFFER,(STRPTR)MSG_OPTIONS_PRTBUFFER_STR},
    {MSG_OPTIONS_FROM,(STRPTR)MSG_OPTIONS_FROM_STR},
    {MSG_OPTIONS_TO,(STRPTR)MSG_OPTIONS_TO_STR},
    {MSG_OPTIONS_NUMBER,(STRPTR)MSG_OPTIONS_NUMBER_STR},
    {MSG_OPTIONS_ODD,(STRPTR)MSG_OPTIONS_ODD_STR},
    {MSG_OPTIONS_EVEN,(STRPTR)MSG_OPTIONS_EVEN_STR},
    {MSG_OPTIONS_PHYSICAL,(STRPTR)MSG_OPTIONS_PHYSICAL_STR},
    {MSG_OPTIONS_HOFFSET,(STRPTR)MSG_OPTIONS_HOFFSET_STR},
    {MSG_OPTIONS_HVOFFSET2,(STRPTR)MSG_OPTIONS_HVOFFSET2_STR},
    {MSG_OPTIONS_VOFFSET,(STRPTR)MSG_OPTIONS_VOFFSET_STR},
    {MSG_OPTIONS_PRINTER,(STRPTR)MSG_OPTIONS_PRINTER_STR},
    {MSG_OPTIONS_OPTIMIZE,(STRPTR)MSG_OPTIONS_OPTIMIZE_STR},
    {MSG_OPTIONS_DRAFT,(STRPTR)MSG_OPTIONS_DRAFT_STR},
    {MSG_OPTIONS_DENSITY,(STRPTR)MSG_OPTIONS_DENSITY_STR},
    {MSG_OPTIONS_DENSITY2,(STRPTR)MSG_OPTIONS_DENSITY2_STR},
    {MSG_OPTIONS_UNIDIRECT,(STRPTR)MSG_OPTIONS_UNIDIRECT_STR},
    {MSG_OPTIONS_COPIES,(STRPTR)MSG_OPTIONS_COPIES_STR},
    {MSG_OPTIONS_LANDSCAPE,(STRPTR)MSG_OPTIONS_LANDSCAPE_STR},
    {MSG_OPTIONS_TWOPAGE,(STRPTR)MSG_OPTIONS_TWOPAGE_STR},
    {MSG_OPTIONS_MOFFSET,(STRPTR)MSG_OPTIONS_MOFFSET_STR},
    {MSG_OPTIONS_BOOK,(STRPTR)MSG_OPTIONS_BOOK_STR},
    {MSG_OPTIONS_IFF,(STRPTR)MSG_OPTIONS_IFF_STR},
    {MSG_OPTIONS_SKIPFORMFEED,(STRPTR)MSG_OPTIONS_SKIPFORMFEED_STR},
    {MSG_OPTIONS_REVERSE,(STRPTR)MSG_OPTIONS_REVERSE_STR},
    {MSG_OPTIONS_RESOLUTION,(STRPTR)MSG_OPTIONS_RESOLUTION_STR},
    {MSG_OPTIONS_RESOLUTION2,(STRPTR)MSG_OPTIONS_RESOLUTION2_STR},
    {MSG_OPTIONS_WIDTH,(STRPTR)MSG_OPTIONS_WIDTH_STR},
    {MSG_OPTIONS_HEIGHT,(STRPTR)MSG_OPTIONS_HEIGHT_STR},
    {MSG_OPTIONS_PRELOAD,(STRPTR)MSG_OPTIONS_PRELOAD_STR},
    {MSG_OPTIONS_FAST,(STRPTR)MSG_OPTIONS_FAST_STR},
    {MSG_OPTIONS_FAST2,(STRPTR)MSG_OPTIONS_FAST2_STR},
    {MSG_OPTIONS_MARK,(STRPTR)MSG_OPTIONS_MARK_STR},
    {MSG_OPTIONS_STATISTIC,(STRPTR)MSG_OPTIONS_STATISTIC_STR},
    {MSG_OPTIONS_DEBUGSTAT,(STRPTR)MSG_OPTIONS_DEBUGSTAT_STR},
    {MSG_OPTIONS_NOLOG,(STRPTR)MSG_OPTIONS_NOLOG_STR},
    {MSG_OPTIONS_OUTTO,(STRPTR)MSG_OPTIONS_OUTTO_STR},
    {MSG_OPTIONS_OUTTO2,(STRPTR)MSG_OPTIONS_OUTTO2_STR},
    {MSG_OPTIONS_LOGNAME,(STRPTR)MSG_OPTIONS_LOGNAME_STR},
    {MSG_OPTIONS_SHOWPRINTERS,(STRPTR)MSG_OPTIONS_SHOWPRINTERS_STR},
    {MSG_OPTIONS_ACCOUNTING,(STRPTR)MSG_OPTIONS_ACCOUNTING_STR},
    {MSG_OPTIONS_PRIORITY,(STRPTR)MSG_OPTIONS_PRIORITY_STR},
    {MSG_OPTIONS_SPECIALHOST,(STRPTR)MSG_OPTIONS_SPECIALHOST_STR},
    {MSG_OPTIONS_PRINTAUTHOR,(STRPTR)MSG_OPTIONS_PRINTAUTHOR_STR},
    {MSG_OPTIONS_DVIFILE,(STRPTR)MSG_OPTIONS_DVIFILE_STR},
    {MSG_OPTIONS_PUBSCREEN,(STRPTR)MSG_OPTIONS_PUBSCREEN_STR},
    {MSG_OPTIONS_PAPER,(STRPTR)MSG_OPTIONS_PAPER_STR},
    {MSG_OPTIONS_PAPER2,(STRPTR)MSG_OPTIONS_PAPER2_STR},
    {MSG_MUI_MW_DESCRIPTION,(STRPTR)MSG_MUI_MW_DESCRIPTION_STR},
    {MSG_MUI_MW_TITEL,(STRPTR)MSG_MUI_MW_TITEL_STR},
    {MSG_MUI_MW_DVIFILE,(STRPTR)MSG_MUI_MW_DVIFILE_STR},
    {MSG_MUI_MW_ASL_DVIFILE,(STRPTR)MSG_MUI_MW_ASL_DVIFILE_STR},
    {MSG_MUI_MW_SEITEN,(STRPTR)MSG_MUI_MW_SEITEN_STR},
    {MSG_MUI_MW_VON,(STRPTR)MSG_MUI_MW_VON_STR},
    {MSG_MUI_MW_BIS,(STRPTR)MSG_MUI_MW_BIS_STR},
    {MSG_MUI_MW_NUM,(STRPTR)MSG_MUI_MW_NUM_STR},
    {MSG_MUI_MW_KOPIEN,(STRPTR)MSG_MUI_MW_KOPIEN_STR},
    {MSG_MUI_MW_DRUCKEN,(STRPTR)MSG_MUI_MW_DRUCKEN_STR},
    {MSG_MUI_MW_PREF,(STRPTR)MSG_MUI_MW_PREF_STR},
    {MSG_MUI_MW_CANCEL,(STRPTR)MSG_MUI_MW_CANCEL_STR},
    {MSG_MUI_MW_APP_FAIL,(STRPTR)MSG_MUI_MW_APP_FAIL_STR},
    {MSG_MUI_MW_SEITEN_ALL,(STRPTR)MSG_MUI_MW_SEITEN_ALL_STR},
    {MSG_MUI_MW_SEITEN_VONBIS,(STRPTR)MSG_MUI_MW_SEITEN_VONBIS_STR},
    {MSG_MUI_ME_WINTITLE,(STRPTR)MSG_MUI_ME_WINTITLE_STR},
    {MSG_MUI_ME_CLOSE,(STRPTR)MSG_MUI_ME_CLOSE_STR},
    {MSG_MUI_ME_TITEL,(STRPTR)MSG_MUI_ME_TITEL_STR},
    {MSG_MUI_WO_WINTITLE,(STRPTR)MSG_MUI_WO_WINTITLE_STR},
    {MSG_MUI_WO_TITEL,(STRPTR)MSG_MUI_WO_TITEL_STR},
    {MSG_MUI_WO_SEEMESS,(STRPTR)MSG_MUI_WO_SEEMESS_STR},
    {MSG_MUI_WORK_DOALL,(STRPTR)MSG_MUI_WORK_DOALL_STR},
    {MSG_MUI_WORK_START,(STRPTR)MSG_MUI_WORK_START_STR},
    {MSG_MUI_WORK_END,(STRPTR)MSG_MUI_WORK_END_STR},
    {MSG_MUI_WORK_FROM,(STRPTR)MSG_MUI_WORK_FROM_STR},
    {MSG_MUI_WORK_PAGE,(STRPTR)MSG_MUI_WORK_PAGE_STR},
    {MSG_MUI_WORK_TO,(STRPTR)MSG_MUI_WORK_TO_STR},
    {MSG_MUI_FA_WINTITLE,(STRPTR)MSG_MUI_FA_WINTITLE_STR},
    {MSG_MUI_FA_TITEL,(STRPTR)MSG_MUI_FA_TITEL_STR},
};

#endif /* CATCOMP_ARRAY */


/****************************************************************************/


#ifdef CATCOMP_BLOCK

static const char CatCompBlock[] =
{
    "\x00\x00\x00\x01\x00\x06"
    MSG_CATALOG_VERSION_STR "\x00"
    "\x00\x00\x00\x02\x00\x2C"
    MSG_WRONG_CATALOG_VERSION_STR "\x00\x00"
    "\x00\x00\x00\x64\x00\x08"
    MSG_PROJECT_MENU_STR "\x00"
    "\x00\x00\x00\x65\x00\x08"
    MSG_OUTFIT_MENU_STR "\x00\x00"
    "\x00\x00\x00\x66\x00\x06"
    MSG_MOVE_MENU_STR "\x00\x00"
    "\x00\x00\x00\x67\x00\x0C"
    MSG_RESOLUTION_MENU_STR "\x00\x00"
    "\x00\x00\x00\x6E\x00\x0A"
    MSG_PROJECT_ABOUT_STR "\x00"
    "\x00\x00\x00\x6F\x00\x0C"
    MSG_PROJECT_OPENAGAIN_STR "\x00\x00"
    "\x00\x00\x00\x70\x00\x0E"
    MSG_PROJECT_OPENNEW_STR "\x00\x00"
    "\x00\x00\x00\x71\x00\x10"
    MSG_PROJECT_AUTOLOADAGAIN_STR "\x00"
    "\x00\x00\x00\x72\x00\x16"
    MSG_PROJECT_SAVEIFF_STR "\x00\x00"
    "\x00\x00\x00\x73\x00\x10"
    MSG_PROJECT_SHELLCOMANDS_STR "\x00\x00"
    "\x00\x00\x00\x74\x00\x18"
    MSG_PROJECT_PRINTPAGE_STR "\x00"
    "\x00\x00\x00\x75\x00\x0C"
    MSG_PROJECT_WBTOFRONT_STR "\x00"
    "\x00\x00\x00\x76\x00\x06"
    MSG_PROJECT_HIDE_STR "\x00\x00"
    "\x00\x00\x00\x77\x00\x0C"
    MSG_PROJECT_SAVECONFIG_STR "\x00"
    "\x00\x00\x00\x78\x00\x06"
    MSG_PROJECT_QUIT_STR "\x00\x00"
    "\x00\x00\x00\x82\x00\x08"
    MSG_PROJECT_SHELLCOMANDS_NEWCLI_STR "\x00"
    "\x00\x00\x00\x83\x00\x10"
    MSG_PROJECT_SHELLCOMANDS_EXECUTECOMMAND_STR "\x00"
    "\x00\x00\x00\x84\x00\x0C"
    MSG_PROJECT_SHELLCOMANDS_TEXSCRIPT_STR "\x00\x00"
    "\x00\x00\x00\x85\x00\x10"
    MSG_PROJECT_SHELLCOMANDS_AREXXTEXSHELL_STR "\x00"
    "\x00\x00\x00\x86\x00\x12"
    MSG_PROJECT_SHELLCOMANDS_SETENVTEXFORMAT_STR "\x00"
    "\x00\x00\x00\x87\x00\x0C"
    MSG_PROJECT_SHELLCOMANDS_SPECIALHOST_STR "\x00"
    "\x00\x00\x00\x8C\x00\x06"
    MSG_OUTFIT_COPY_STR "\x00\x00"
    "\x00\x00\x00\x8D\x00\x06"
    MSG_OUTFIT_LACE_STR "\x00\x00"
    "\x00\x00\x00\x8E\x00\x0A"
    MSG_OUTFIT_SCROLLBAR_STR "\x00"
    "\x00\x00\x00\x8F\x00\x0A"
    MSG_OUTFIT_FULLPAGE_STR "\x00"
    "\x00\x00\x00\x90\x00\x10"
    MSG_OUTFIT_MEASUREWINDOW_STR "\x00\x00"
    "\x00\x00\x00\x91\x00\x0C"
    MSG_OUTFIT_BORDERLINE_STR "\x00"
    "\x00\x00\x00\x92\x00\x0C"
    MSG_OUTFIT_SETMARGIN_STR "\x00"
    "\x00\x00\x00\x93\x00\x10"
    MSG_OUTFIT_4COLORSCREEN_STR "\x00\x00"
    "\x00\x00\x00\x94\x00\x10"
    MSG_OUTFIT_PAGESCROLLBAR_STR "\x00\x00"
    "\x00\x00\x00\x95\x00\x06"
    MSG_OUTFIT_UNIT_STR "\x00\x00"
    "\x00\x00\x00\x96\x00\x06"
    MSG_OUTFIT_COLOR_STR "\x00"
    "\x00\x00\x00\x97\x00\x10"
    MSG_OUTFIT_CLONEWBCOLOR_STR "\x00\x00"
    "\x00\x00\x00\x98\x00\x12"
    MSG_OUTFIT_SCREENPREFS_STR "\x00\x00"
    "\x00\x00\x00\xA0\x00\x06"
    MSG_OUTFIT_UNIT_INCH_STR "\x00\x00"
    "\x00\x00\x00\xA1\x00\x04"
    MSG_OUTFIT_UNIT_CM_STR "\x00\x00"
    "\x00\x00\x00\xA2\x00\x04"
    MSG_OUTFIT_UNIT_PT_STR "\x00\x00"
    "\x00\x00\x00\xAA\x00\x12"
    MSG_MOVE_SEARCH_STR "\x00"
    "\x00\x00\x00\xAB\x00\x0A"
    MSG_MOVE_PREVPAGE_STR "\x00"
    "\x00\x00\x00\xAC\x00\x0A"
    MSG_MOVE_NEXTPAGE_STR "\x00"
    "\x00\x00\x00\xAD\x00\x0C"
    MSG_MOVE_FIRSTPAGE_STR "\x00\x00"
    "\x00\x00\x00\xAE\x00\x0A"
    MSG_MOVE_LASTPAGE_STR "\x00"
    "\x00\x00\x00\xAF\x00\x14"
    MSG_MOVE_PAGECOUNTER_STR "\x00\x00"
    "\x00\x00\x00\xB0\x00\x14"
    MSG_MOVE_JUMPTOPAGENUMBER_STR "\x00"
    "\x00\x00\x00\xB1\x00\x14"
    MSG_MOVE_CLEARPAGECOUNTER_STR "\x00\x00"
    "\x00\x00\x00\xB2\x00\x1E"
    MSG_MOVE_USEPHY_STR "\x00"
    "\x00\x00\x00\xB3\x00\x1A"
    MSG_MOVE_USEORDERDVI_STR "\x00"
    "\x00\x00\x00\xB4\x00\x0A"
    MSG_MOVE_USEPHYPREVPAGE_STR "\x00"
    "\x00\x00\x00\xB5\x00\x0A"
    MSG_MOVE_USEPHYNEXTPAGE_STR "\x00"
    "\x00\x00\x00\xB6\x00\x0C"
    MSG_MOVE_USEPHYFIRSTPAGE_STR "\x00\x00"
    "\x00\x00\x00\xB7\x00\x0A"
    MSG_MOVE_USEPHYLASTPAGE_STR "\x00"
    "\x00\x00\x01\xF4\x00\x2A"
    MSG_NO_LIBRARY_STR "\x00"
    "\x00\x00\x01\xF5\x00\x22"
    MSG_NO_CONSOLE_DEVICE_STR "\x00"
    "\x00\x00\x01\xF6\x00\x1C"
    MSG_NO_NEW_OBJECT_STR "\x00\x00"
    "\x00\x00\x01\xF7\x00\x46"
    MSG_CANT_CLOSE_SCREEN_STR "\x00"
    "\x00\x00\x01\xF8\x00\x28"
    MSG_CLOSE_WINDOWS_STR "\x00\x00"
    "\x00\x00\x01\xF9\x00\x0A"
    MSG_SCRERR_NOERR_STR "\x00"
    "\x00\x00\x01\xFA\x00\x28"
    MSG_SCRERR_NOMONITOR_STR "\x00\x00"
    "\x00\x00\x01\xFB\x00\x2E"
    MSG_SCRERR_NOCHIPS_STR "\x00\x00"
    "\x00\x00\x01\xFC\x00\x1C"
    MSG_SCRERR_NOMEM_STR "\x00\x00"
    "\x00\x00\x01\xFD\x00\x1A"
    MSG_SCRERR_NOCHIPMEM_STR "\x00\x00"
    "\x00\x00\x01\xFE\x00\x20"
    MSG_SCRERR_PUBNOTUNIQUE_STR "\x00"
    "\x00\x00\x01\xFF\x00\x28"
    MSG_SCRERR_UNKNOWNMODE_STR "\x00\x00"
    "\x00\x00\x02\x00\x00\x0E"
    MSG_SCRERR_DEFAULT_STR "\x00"
    "\x00\x00\x02\x01\x00\x1A"
    MSG_CANT_OPEN_SCR_NAME_STR "\x00\x00"
    "\x00\x00\x02\x02\x00\x36"
    MSG_CANT_FIND_SCR_USE_WB_STR "\x00\x00"
    "\x00\x00\x02\x03\x00\x1A"
    MSG_CANT_LOCK_PBSCR_STR "\x00"
    "\x00\x00\x02\x04\x00\x18"
    MSG_CANT_GET_VI_STR "\x00\x00"
    "\x00\x00\x02\x05\x00\x16"
    MSG_CANT_GET_DI_STR "\x00\x00"
    "\x00\x00\x02\x06\x00\x14"
    MSG_CANT_OPEN_WIN_STR "\x00\x00"
    "\x00\x00\x02\x07\x00\x24"
    MSG_SET_MARGIN_STR "\x00"
    "\x00\x00\x02\x08\x00\x1E"
    MSG_SEARCH_STRING_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x02\x09\x00\x12"
    MSG_SEARCH_STRING_CANCELED_STR "\x00\x00"
    "\x00\x00\x02\x0A\x00\x12"
    MSG_SHOWDVI_MESSAGE_STR "\x00\x00"
    "\x00\x00\x02\x0B\x00\x12"
    MSG_ABORT_PRINT_PAGE_STR "\x00"
    "\x00\x00\x02\x0C\x00\x0A"
    MSG_OK_CANCEL_REQSTRING_STR "\x00"
    "\x00\x00\x02\x0D\x00\x12"
    MSG_ABORT_PRINT_STR "\x00"
    "\x00\x00\x02\x0E\x00\x10"
    MSG_PRINT_IS_ABORTED_STR "\x00\x00"
    "\x00\x00\x02\x0F\x00\x14"
    MSG_PRINT_CUR_PAGE_STR "\x00"
    "\x00\x00\x02\x10\x00\x26"
    MSG_WIN_HEADER_FILE_STR "\x00\x00"
    "\x00\x00\x02\x11\x00\x1C"
    MSG_WIN_HEADER_NO_FILE_STR "\x00\x00"
    "\x00\x00\x02\x12\x00\x12"
    MSG_UNAVAILABLE_MODE_STR "\x00\x00"
    "\x00\x00\x02\x13\x00\x12"
    MSG_UNKNOWN_MODE_NAME_STR "\x00"
    "\x00\x00\x02\x14\x00\x10"
    MSG_INTERNAL_ERROR_STR "\x00"
    "\x00\x00\x02\x15\x00\x18"
    MSG_NO_CHIPMEM_STR "\x00"
    "\x00\x00\x02\x16\x00\x16"
    MSG_BUILD_FULL_PAGE_STR "\x00"
    "\x00\x00\x02\x17\x00\x14"
    MSG_NO_MEM_STR "\x00\x00"
    "\x00\x00\x02\x18\x00\x22"
    MSG_CANT_SAVE_TO_CLIP_STR "\x00"
    "\x00\x00\x02\x19\x00\x22"
    MSG_CANT_SAVE_TO_IFF_STR "\x00"
    "\x00\x00\x02\x1A\x00\x16"
    MSG_NO_SIGNAL_STR "\x00\x00"
    "\x00\x00\x02\x1B\x00\x3E"
    MSG_CANT_NOTIFY_STR "\x00\x00"
    "\x00\x00\x02\x1C\x00\x16"
    MSG_MARGIN_SET_STR "\x00\x00"
    "\x00\x00\x02\x1D\x00\x36"
    MSG_SHOWDVI_ALREADY_ACTIVE_STR "\x00"
    "\x00\x00\x02\x1E\x00\x18"
    MSG_CANT_ACCESS_FILE_STR "\x00"
    "\x00\x00\x02\x1F\x00\x20"
    MSG_AREXX_COMM_START_FAILED_STR "\x00\x00"
    "\x00\x00\x02\x20\x00\x30"
    MSG_AREXX_COMM_FAILED_STR "\x00\x00"
    "\x00\x00\x02\x21\x00\x14"
    MSG_DIV_ZERO_STR "\x00\x00"
    "\x00\x00\x02\x22\x00\x1E"
    MSG_CANT_SET_VARIABLE_STR "\x00"
    "\x00\x00\x02\x23\x00\x28"
    MSG_ERROR_IN_CONFIG_FILE_STR "\x00\x00"
    "\x00\x00\x02\x24\x00\x28"
    MSG_BETA_VERSION_STR "\x00"
    "\x00\x00\x02\x25\x00\x16"
    MSG_INTERNAL_ERROR_W_ARG_STR "\x00"
    "\x00\x00\x02\x26\x00\x14"
    MSG_WRONG_MAP_WIDTH_STR "\x00"
    "\x00\x00\x02\x27\x00\x14"
    MSG_NO_DVI_FILE_STR "\x00"
    "\x00\x00\x02\x28\x00\x18"
    MSG_DVI_FILE_LOST_STR "\x00\x00"
    "\x00\x00\x02\x29\x00\x14"
    MSG_UNKNOWN_POP_SUBMENU_STR "\x00\x00"
    "\x00\x00\x02\x2A\x00\x0A"
    MSG_FILENAME_STR "\x00"
    "\x00\x00\x02\x2B\x00\x20"
    MSG_ENTER_COMMAND_STR "\x00"
    "\x00\x00\x02\x2C\x00\x18"
    MSG_EXECUTE_COMMAND_STR "\x00\x00"
    "\x00\x00\x02\x2D\x00\x18"
    MSG_CANT_EXECUTE_COMMAND_STR "\x00\x00"
    "\x00\x00\x02\x2E\x00\x1A"
    MSG_USE_WHICH_PUBSCR_STR "\x00\x00"
    "\x00\x00\x02\x2F\x00\x3E"
    MSG_DONT_FIND_PUBSCR_USE_WB_STR "\x00\x00"
    "\x00\x00\x02\x30\x00\x12"
    MSG_SHOWDVI_TEX_SHELL_STR "\x00"
    "\x00\x00\x02\x31\x00\x12"
    MSG_CANT_START_STR "\x00"
    "\x00\x00\x02\x32\x00\x12"
    MSG_START_SPECIALHOST_STR "\x00"
    "\x00\x00\x02\x33\x00\x1E"
    MSG_UNKNOWN_SUBITEM_STR "\x00\x00"
    "\x00\x00\x02\x34\x00\x1E"
    MSG_UNKNOWN_SUBMENU_STR "\x00\x00"
    "\x00\x00\x02\x35\x00\x1A"
    MSG_UNKNOWN_MENU_STR "\x00"
    "\x00\x00\x02\x36\x00\x20"
    MSG_CANT_OPEN_HELP_FILE_STR "\x00\x00"
    "\x00\x00\x02\x37\x00\x12"
    MSG_SHOWDVI_MENU_HELP_STR "\x00"
    "\x00\x00\x02\x38\x00\x26"
    MSG_NO_HELP_FOR_MENU_STR "\x00"
    "\x00\x00\x02\x39\x00\x38"
    MSG_INTERNAL_ERROR_MENU_ENTRY_STR "\x00"
    "\x00\x00\x02\x3A\x00\x1A"
    MSG_CONFUSED_ABOUT_REQ_STR "\x00"
    "\x00\x00\x02\x3B\x00\x1A"
    MSG_SHOWDVI_ABOUT_WINDOW_STR "\x00"
    "\x00\x00\x02\x3C\x00\x24"
    MSG_CANT_OPEN_ABOUT_REQ_STR "\x00\x00"
    "\x00\x00\x02\x3D\x00\x1A"
    MSG_NEED_REQ_LIB_STR "\x00"
    "\x00\x00\x02\x3E\x00\x10"
    MSG_SHOWDVI_REQUEST_STR "\x00"
    "\x00\x00\x02\x3F\x00\x04"
    MSG_OK_STR "\x00\x00"
    "\x00\x00\x02\x40\x00\x08"
    MSG_CANCEL_STR "\x00\x00"
    "\x00\x00\x02\x41\x00\x16"
    MSG_SHOWDVI_FATAL_MESSAGE_STR "\x00"
    "\x00\x00\x02\x42\x00\x2A"
    MSG_FATAL_WRONG_STR "\x00"
    "\x00\x00\x02\x43\x00\x30"
    MSG_FATAL_CHIPMEM_STR "\x00\x00"
    "\x00\x00\x02\x44\x00\x28"
    MSG_FATAL_MEMORY_STR "\x00"
    "\x00\x00\x02\x45\x00\x2A"
    MSG_FATAL_INTERNAL_STR "\x00"
    "\x00\x00\x02\x46\x00\x32"
    MSG_FATAL_FATAL_STR "\x00\x00"
    "\x00\x00\x02\x47\x00\x0E"
    MSG_REALLY_QUIT_STR "\x00\x00"
    "\x00\x00\x02\x48\x00\x08"
    MSG_YES_NO_STR "\x00\x00"
    "\x00\x00\x02\x49\x00\x20"
    MSG_FILE_ISNT_DVI_FILE_STR "\x00"
    "\x00\x00\x02\x4A\x00\x26"
    MSG_CANT_ALLOC_FREQ_STR "\x00\x00"
    "\x00\x00\x02\x4B\x00\x12"
    MSG_LOAD_NEW_DVI_STR "\x00"
    "\x00\x00\x02\x4C\x00\x18"
    MSG_CANT_FOUND_FILE_STR "\x00\x00"
    "\x00\x00\x02\x4D\x00\x1E"
    MSG_CANT_ALLOC_ASLREQ_STR "\x00\x00"
    "\x00\x00\x02\x4E\x00\x24"
    MSG_FORMAT_FILE_NAME_STR "\x00"
    "\x00\x00\x02\x4F\x00\x24"
    MSG_NOT_WHILE_PRINTING_STR "\x00"
    "\x00\x00\x02\x50\x00\x1E"
    MSG_NO_APP_ICON_STR "\x00"
    "\x00\x00\x02\x51\x00\x10"
    MSG_ENTER_A_STRING_STR "\x00\x00"
    "\x00\x00\x02\x52\x00\x10"
    MSG_ENTER_A_NUMBER_STR "\x00\x00"
    "\x00\x00\x02\x53\x00\x30"
    MSG_CANT_OPEN_CONFIG_STR "\x00"
    "\x00\x00\x02\x54\x00\x2A"
    MSG_LOAD_CONFIG_STR "\x00\x00"
    "\x00\x00\x02\x55\x00\x2A"
    MSG_WRITE_CONFIG_STR "\x00\x00"
    "\x00\x00\x02\x56\x00\x26"
    MSG_NO_AREXX_FOR_FX_STR "\x00"
    "\x00\x00\x02\x57\x00\x18"
    MSG_NO_ON_OFF_STR "\x00\x00"
    "\x00\x00\x02\x58\x00\x24"
    MSG_WRONG_ON_OFF_STR "\x00"
    "\x00\x00\x02\x59\x00\x26"
    MSG_UNKNOWN_KEYWORD_STR "\x00\x00"
    "\x00\x00\x02\x5A\x00\x1A"
    MSG_UNKNOWN_MODEID_STR "\x00\x00"
    "\x00\x00\x02\x5B\x00\x16"
    MSG_ILLEGAL_COLOR_STR "\x00\x00"
    "\x00\x00\x02\x5C\x00\x1C"
    MSG_WRONG_COLOR_PARAMS_STR "\x00\x00"
    "\x00\x00\x02\x5D\x00\x2C"
    MSG_WRONG_DEF_RESO_STR "\x00\x00"
    "\x00\x00\x02\x5E\x00\x28"
    MSG_WRONG_VAL_IN_RES_MENU_STR "\x00\x00"
    "\x00\x00\x02\x5F\x00\x2A"
    MSG_WRONG_UNITS_STR "\x00\x00"
    "\x00\x00\x02\x60\x00\x20"
    MSG_WRONG_APPICON_POS_STR "\x00"
    "\x00\x00\x02\x61\x00\x2A"
    MSG_WRONG_MIN_SCREEN_SIZE_STR "\x00\x00"
    "\x00\x00\x02\x62\x00\x1A"
    MSG_WRONG_SCREEN_SIZE_STR "\x00"
    "\x00\x00\x02\x63\x00\x1C"
    MSG_WRONG_MONITOR_SIZE_STR "\x00\x00"
    "\x00\x00\x02\x64\x00\x1E"
    MSG_WRONG_WIN_POS_STR "\x00"
    "\x00\x00\x02\x65\x00\x1A"
    MSG_WRONG_WIN_SIZE_STR "\x00"
    "\x00\x00\x02\x66\x00\x22"
    MSG_WRONG_MAX_DVIBUF_SIZE_STR "\x00"
    "\x00\x00\x02\x67\x00\x42"
    MSG_CONFIG_HEADER_STR "\x00"
    "\x00\x00\x02\x68\x00\x20"
    MSG_CONFIG_RGB_BACK_STR "\x00\x00"
    "\x00\x00\x02\x69\x00\x20"
    MSG_CONFIG_RGB_FORE_STR "\x00\x00"
    "\x00\x00\x02\x6A\x00\x16"
    MSG_CONFIG_RGB_2_STR "\x00"
    "\x00\x00\x02\x6B\x00\x16"
    MSG_CONFIG_RGB_3_STR "\x00"
    "\x00\x00\x02\x6C\x00\x14"
    MSG_CONFIG_SCROLL_STATE_STR "\x00"
    "\x00\x00\x02\x6D\x00\x12"
    MSG_CONFIG_BORDER_LINE_STR "\x00\x00"
    "\x00\x00\x02\x6E\x00\x0A"
    MSG_CONFIG_LACE_STR "\x00"
    "\x00\x00\x02\x6F\x00\x12"
    MSG_CONFIG_BEEP_STR "\x00\x00"
    "\x00\x00\x02\x70\x00\x12"
    MSG_CONFIG_ESC_STR "\x00\x00"
    "\x00\x00\x02\x71\x00\x16"
    MSG_CONFIG_QUICK_EXIT_STR "\x00"
    "\x00\x00\x02\x72\x00\x10"
    MSG_CONFIG_POPUP_STR "\x00"
    "\x00\x00\x02\x73\x00\x22"
    MSG_CONFIG_INT_POPUP_STR "\x00\x00"
    "\x00\x00\x02\x74\x00\x1A"
    MSG_CONFIG_BIG_POPUP_STR "\x00"
    "\x00\x00\x02\x75\x00\x28"
    MSG_CONFIG_MIDDLE_POPUP_STR "\x00"
    "\x00\x00\x02\x76\x00\x1E"
    MSG_CONFIG_USEPHY_STR "\x00\x00"
    "\x00\x00\x02\x77\x00\x24"
    MSG_CONFIG_ACT_LOAD_AGAIN_STR "\x00\x00"
    "\x00\x00\x02\x78\x00\x10"
    MSG_CONFIG_USE_OWN_SCR_STR "\x00\x00"
    "\x00\x00\x02\x79\x00\x3C"
    MSG_CONFIG_SHOW_PUBSCR_NAME_STR "\x00"
    "\x00\x00\x02\x7A\x00\x30"
    MSG_CONFIG_DEF_PUBSCR_NAME_STR "\x00\x00"
    "\x00\x00\x02\x7B\x00\x38"
    MSG_CONFIG_WIN_POS_STR "\x00"
    "\x00\x00\x02\x7C\x00\x36"
    MSG_CONFIG_WIN_SIZE_STR "\x00\x00"
    "\x00\x00\x02\x7D\x00\x3C"
    MSG_CONFIG_SCR_POS_STR "\x00\x00"
    "\x00\x00\x02\x7E\x00\x3C"
    MSG_CONFIG_SHOW_WIN_SIZE_STR "\x00\x00"
    "\x00\x00\x02\x7F\x00\x14"
    MSG_CONFIG_4_COL_SCR_STR "\x00\x00"
    "\x00\x00\x02\x80\x00\x1A"
    MSG_CONFIG_BACK_HOOK_STR "\x00"
    "\x00\x00\x02\x81\x00\x14"
    MSG_CONFIG_USE_WB_COLS_STR "\x00"
    "\x00\x00\x02\x82\x00\x0A"
    MSG_CONFIG_UNIT_STR "\x00"
    "\x00\x00\x02\x83\x00\x18"
    MSG_CONFIG_APP_ICON_STR "\x00"
    "\x00\x00\x02\x84\x00\x1E"
    MSG_CONFIG_INFO_APP_ICON_STR "\x00"
    "\x00\x00\x02\x85\x00\x26"
    MSG_CONFIG_APP_ICON_POS_STR "\x00\x00"
    "\x00\x00\x02\x86\x00\x1E"
    MSG_CONIFG_SCRIPT_FILE_STR "\x00"
    "\x00\x00\x02\x87\x00\x34"
    MSG_CONFIG_TEX_SERVER_STR "\x00"
    "\x00\x00\x02\x88\x00\x22"
    MSG_CONFIG_SCR_SIZE_STR "\x00"
    "\x00\x00\x02\x89\x00\x40"
    MSG_CONFIG_SCR_MODE_STR "\x00\x00"
    "\x00\x00\x02\x8A\x00\x14"
    MSG_CONFIG_MONITOR_SIZE_STR "\x00"
    "\x00\x00\x02\x8B\x00\x36"
    MSG_CONFIG_ALWBMFAST_STR "\x00"
    "\x00\x00\x02\x8C\x00\x3A"
    MSG_CONFIG_SMARTWIN_STR "\x00\x00"
    "\x00\x00\x02\x8D\x00\x3C"
    MSG_CONFIG_MAX_DVIBUF_STR "\x00\x00"
    "\x00\x00\x02\x8E\x00\x1A"
    MSG_CONFIG_DEF_RESO_STR "\x00\x00"
    "\x00\x00\x02\x8F\x00\x20"
    MSG_CONFIG_RESO_MENU_STR "\x00"
    "\x00\x00\x02\x90\x00\x16"
    MSG_CONFIG_SAVED_OK_STR "\x00\x00"
    "\x00\x00\x02\x91\x00\x1A"
    MSG_CANT_SAVE_CONFIG_STR "\x00"
    "\x00\x00\x02\x92\x00\x2A"
    MSG_CONFIG_APEN_STR "\x00\x00"
    "\x00\x00\x02\x93\x00\x2A"
    MSG_CONFIG_BPEN_STR "\x00\x00"
    "\x00\x00\x02\x94\x00\x20"
    MSG_WRONG_PEN_STR "\x00\x00"
    "\x00\x00\x02\x95\x00\x10"
    MSG_MEASURE_WINDOW_STR "\x00\x00"
    "\x00\x00\x02\x96\x00\x14"
    MSG_CANT_OPEN_MESSWIN_STR "\x00"
    "\x00\x00\x02\x97\x00\x08"
    MSG_MESSWIN_WIDTH_STR "\x00\x00"
    "\x00\x00\x02\x98\x00\x08"
    MSG_MESSWIN_HEIGHT_STR "\x00"
    "\x00\x00\x02\x99\x00\x0A"
    MSG_MESSWIN_DEL_X_STR "\x00\x00"
    "\x00\x00\x02\x9A\x00\x0A"
    MSG_MESSWIN_X_STR "\x00\x00"
    "\x00\x00\x02\x9B\x00\x0A"
    MSG_MESSWIN_DEL_Y_STR "\x00\x00"
    "\x00\x00\x02\x9C\x00\x0A"
    MSG_MESSWIN_Y_STR "\x00\x00"
    "\x00\x00\x02\x9D\x00\x14"
    MSG_MESSWIN_MAG_STR "\x00\x00"
    "\x00\x00\x02\x9E\x00\x18"
    MSG_AREXX_REPLY_CODE_STR "\x00"
    "\x00\x00\x02\x9F\x00\x1E"
    MSG_EXECUTE_TEX_SCRIPT_STR "\x00\x00"
    "\x00\x00\x02\xA0\x00\x12"
    MSG_NEWCLI_TITLE_STR "\x00"
    "\x00\x00\x02\xA1\x00\x10"
    MSG_PREFWIN_GADARR_0_STR "\x00\x00"
    "\x00\x00\x02\xA2\x00\x10"
    MSG_PREFWIN_GADARR_1_STR "\x00"
    "\x00\x00\x02\xA3\x00\x0E"
    MSG_PREFWIN_GADARR_2_STR "\x00\x00"
    "\x00\x00\x02\xA4\x00\x10"
    MSG_PREFWIN_GADARR_3_STR "\x00\x00"
    "\x00\x00\x02\xA5\x00\x10"
    MSG_PREFWIN_GADARR_4_STR "\x00\x00"
    "\x00\x00\x02\xA6\x00\x06"
    MSG_PREFWIN_GADARR_5_STR "\x00\x00"
    "\x00\x00\x02\xA7\x00\x08"
    MSG_PREFWIN_GADARR_6_STR "\x00"
    "\x00\x00\x02\xA8\x00\x10"
    MSG_PREFWIN_GADARR_7_STR "\x00"
    "\x00\x00\x02\xA9\x00\x10"
    MSG_PREFWIN_GADARR_8_STR "\x00\x00"
    "\x00\x00\x02\xAA\x00\x14"
    MSG_PREFWIN_GADARR_9_STR "\x00"
    "\x00\x00\x02\xAB\x00\x0E"
    MSG_PREFWIN_GADARR_10_STR "\x00\x00"
    "\x00\x00\x02\xAC\x00\x62"
    MSG_PREFWIN_GADTOOLS_STR "\x00"
    "\x00\x00\x02\xAD\x00\x14"
    MSG_PREFWIN_WIN_TITLE_STR "\x00\x00"
    "\x00\x00\x02\xAE\x00\x26"
    MSG_PREFWIN_CANT_OPEN_STR "\x00\x00"
    "\x00\x00\x02\xAF\x00\x16"
    MSG_SEARCHWIN_WIN_TITLE_STR "\x00"
    "\x00\x00\x02\xB0\x00\x16"
    MSG_SEARCHWIN_DOSEARCH_STR "\x00"
    "\x00\x00\x02\xB1\x00\x0E"
    MSG_SEARCHWIN_FOUND_STR "\x00"
    "\x00\x00\x02\xB2\x00\x1A"
    MSG_SEARCHWIN_CANT_OPEN_STR "\x00"
    "\x00\x00\x02\xB3\x00\x14"
    MSG_SEARCHWIN_STRING_STR "\x00\x00"
    "\x00\x00\x02\xB4\x00\x08"
    MSG_SEARCHWIN_SEARCH_STR "\x00"
    "\x00\x00\x02\xB5\x00\x08"
    MSG_SEARCHWIN_CANCEL_STR "\x00"
    "\x00\x00\x02\xB6\x00\x14"
    MSG_NO_FILE_GIVEN_STR "\x00\x00"
    "\x00\x00\x02\xB7\x00\x14"
    MSG_BUILD_BITMAP_STR "\x00"
    "\x00\x00\x02\xB8\x00\x10"
    MSG_STACK_OVER_STR "\x00\x00"
    "\x00\x00\x02\xB9\x00\x10"
    MSG_STACK_UNDER_STR "\x00"
    "\x00\x00\x02\xBA\x00\x18"
    MSG_PRE_IN_FILE_STR "\x00\x00"
    "\x00\x00\x02\xBB\x00\x22"
    MSG_POST_IN_FILE_STR "\x00"
    "\x00\x00\x02\xBC\x00\x22"
    MSG_POST_POST_STR "\x00\x00"
    "\x00\x00\x02\xBD\x00\x10"
    MSG_DVI_FILE_ERROR_STR "\x00"
    "\x00\x00\x02\xBE\x00\x24"
    MSG_ALREADY_FIRST_STR "\x00\x00"
    "\x00\x00\x02\xBF\x00\x22"
    MSG_ALREADY_LAST_STR "\x00"
    "\x00\x00\x02\xC0\x00\x14"
    MSG_PAGE_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x02\xC1\x00\x3A"
    MSG_MISSING_PRE_STR "\x00"
    "\x00\x00\x02\xC2\x00\x36"
    MSG_WRONG_DVI_TYPE_STR "\x00"
    "\x00\x00\x02\xC3\x00\x18"
    MSG_LOADING_DVI_STR "\x00\x00"
    "\x00\x00\x02\xC4\x00\x2A"
    MSG_INCOMPLETE_DVI_PRELOAD_STR "\x00"
    "\x00\x00\x02\xC5\x00\x16"
    MSG_SCAN_DVI_FILE_STR "\x00"
    "\x00\x00\x02\xC6\x00\x18"
    MSG_HELP_HELP_STR "\x00\x00"
    "\x00\x00\x02\xC7\x00\x26"
    MSG_HELP_FONTDIR_STR "\x00\x00"
    "\x00\x00\x02\xC8\x00\x18"
    MSG_HELP_FONTMEM_STR "\x00\x00"
    "\x00\x00\x02\xC9\x00\x0E"
    MSG_HELP_FROM_STR "\x00"
    "\x00\x00\x02\xCA\x00\x24"
    MSG_HELP_HOFFSET_STR "\x00"
    "\x00\x00\x02\xCB\x00\x2E"
    MSG_HELP_HOFFSET_2_STR "\x00\x00"
    "\x00\x00\x02\xCC\x00\x22"
    MSG_HELP_VOFFSET_STR "\x00"
    "\x00\x00\x02\xCD\x00\x2E"
    MSG_HELP_VOFFSET_2_STR "\x00\x00"
    "\x00\x00\x02\xCE\x00\x12"
    MSG_HELP_PRELOAD_STR "\x00"
    "\x00\x00\x02\xCF\x00\x1C"
    MSG_HELP_RESOLUTION_STR "\x00\x00"
    "\x00\x00\x02\xD0\x00\x1C"
    MSG_HELP_STATISTIC_STR "\x00\x00"
    "\x00\x00\x02\xD1\x00\x20"
    MSG_HELP_DEBUGSTAT_STR "\x00\x00"
    "\x00\x00\x02\xD2\x00\x0E"
    MSG_HELP_LOGNAME_STR "\x00\x00"
    "\x00\x00\x02\xD3\x00\x16"
    MSG_HELP_NOLOG_STR "\x00\x00"
    "\x00\x00\x02\xD4\x00\x16"
    MSG_HELP_PRIORITY_STR "\x00"
    "\x00\x00\x02\xD5\x00\x12"
    MSG_HELP_PRINTAUTHOR_STR "\x00\x00"
    "\x00\x00\x02\xD6\x00\x0A"
    MSG_HELP_DVIFILE_STR "\x00\x00"
    "\x00\x00\x02\xD7\x00\x12"
    MSG_COPYRIGHT_STR "\x00"
    "\x00\x00\x02\xD8\x00\x0C"
    MSG_USAGE_STR "\x00\x00"
    "\x00\x00\x02\xD9\x00\x1A"
    MSG_CANT_FIND_FILE_STR "\x00\x00"
    "\x00\x00\x02\xDA\x00\x1E"
    MSG_NOT_ENOUGH_MEM_BYTES_STR "\x00"
    "\x00\x00\x02\xDB\x00\x26"
    MSG_PROGRAM_END_ERR_STR "\x00\x00"
    "\x00\x00\x02\xDC\x00\x18"
    MSG_USER_ABORT_STR "\x00\x00"
    "\x00\x00\x02\xDD\x00\x24"
    MSG_PROGRAM_END_OK_STR "\x00"
    "\x00\x00\x02\xDE\x00\x12"
    MSG_DVIP_PRINT_FINISHED_STR "\x00\x00"
    "\x00\x00\x02\xDF\x00\x0A"
    MSG_FATAL_STR "\x00\x00"
    "\x00\x00\x02\xE0\x00\x18"
    MSG_LOG_FILE_CREATED_STR "\x00\x00"
    "\x00\x00\x02\xE1\x00\x1C"
    MSG_LINE_BUFFER_OVERFLOW_STR "\x00\x00"
    "\x00\x00\x02\xE2\x00\x0A"
    MSG_BREAK_STR "\x00"
    "\x00\x00\x02\xE3\x00\x24"
    MSG_BREAK_IO_STR "\x00"
    "\x00\x00\x02\xE4\x00\x18"
    MSG_CALL_MF_STR "\x00"
    "\x00\x00\x02\xE5\x00\x12"
    MSG_CANT_OPEN_STR "\x00"
    "\x00\x00\x02\xE6\x00\x3C"
    MSG_DVIP_WHOLE_BITMAP_IN_RAM_STR "\x00\x00"
    "\x00\x00\x02\xE7\x00\x2A"
    MSG_CANT_ALLOC_BITMAP_STR "\x00\x00"
    "\x00\x00\x02\xE8\x00\x1E"
    MSG_DVIP_PARTS_STR "\x00\x00"
    "\x00\x00\x02\xE9\x00\x34"
    MSG_DVIP_SAVE_CLIP_STR "\x00\x00"
    "\x00\x00\x02\xEA\x00\x26"
    MSG_DVIP_SAVE_IFF_STR "\x00\x00"
    "\x00\x00\x02\xEB\x00\x24"
    MSG_DVIP_CANT_SAVE_IFF_STR "\x00\x00"
    "\x00\x00\x02\xEC\x00\x1E"
    MSG_SPECIAL_TOO_LONG_STR "\x00"
    "\x00\x00\x02\xED\x00\x30"
    MSG_NO_MEM_FOR_SPECIAL_STR "\x00\x00"
    "\x00\x00\x02\xEE\x00\x24"
    MSG_PICT_OUT_LEFT_STR "\x00"
    "\x00\x00\x02\xEF\x00\x1E"
    MSG_PICT_OUT_RIGHT_STR "\x00\x00"
    "\x00\x00\x02\xF0\x00\x24"
    MSG_NO_MEM_FOR_SPECIAL_BITMAP_STR "\x00\x00"
    "\x00\x00\x02\xF1\x00\x1C"
    MSG_UNKNOWN_PICT_LOC_STR "\x00\x00"
    "\x00\x00\x02\xF2\x00\x20"
    MSG_ILLEG_PS_COMM_STR "\x00\x00"
    "\x00\x00\x02\xF3\x00\x26"
    MSG_TOO_MANY_POINTS_STR "\x00\x00"
    "\x00\x00\x02\xF4\x00\x24"
    MSG_MALFORMED_PATH_COMM_STR "\x00"
    "\x00\x00\x02\xF5\x00\x14"
    MSG_BAD_DVI_FILE_END_STR "\x00"
    "\x00\x00\x02\xF6\x00\x1C"
    MSG_BAD_FONT_DEFS_STR "\x00"
    "\x00\x00\x02\xF7\x00\x22"
    MSG_MISS_POST_POST_STR "\x00\x00"
    "\x00\x00\x02\xF8\x00\x3A"
    MSG_MISS_POST_STR "\x00"
    "\x00\x00\x02\xF9\x00\x12"
    MSG_LOG_PAGE_DIM_STR "\x00"
    "\x00\x00\x02\xFA\x00\x40"
    MSG_LOG_HORIZ_STR "\x00\x00"
    "\x00\x00\x02\xFB\x00\x40"
    MSG_LOG_VERT_STR "\x00\x00"
    "\x00\x00\x02\xFC\x00\x2E"
    MSG_LOG_MAG_STR "\x00"
    "\x00\x00\x02\xFD\x00\x0A"
    MSG_LOG_OFFSET_STR "\x00\x00"
    "\x00\x00\x02\xFE\x00\x30"
    MSG_LOG_THOFF_STR "\x00"
    "\x00\x00\x02\xFF\x00\x2C"
    MSG_LOG_HOFF_STR "\x00"
    "\x00\x00\x03\x00\x00\x30"
    MSG_LOG_TVOFF_STR "\x00"
    "\x00\x00\x03\x01\x00\x2C"
    MSG_LOG_VOFF_STR "\x00"
    "\x00\x00\x03\x02\x00\x2E"
    MSG_LOG_RESO_STR "\x00\x00"
    "\x00\x00\x03\x03\x00\x30"
    MSG_DVI_STACK_TOO_SMALL_STR "\x00\x00"
    "\x00\x00\x03\x04\x00\x18"
    MSG_LOG_NUM_PAGES_STR "\x00\x00"
    "\x00\x00\x03\x05\x00\x1E"
    MSG_CANT_OPEN_FONTLIB_STR "\x00\x00"
    "\x00\x00\x03\x06\x00\x22"
    MSG_NOT_A_FONTLIB_STR "\x00"
    "\x00\x00\x03\x07\x00\x2C"
    MSG_OLD_FONTLIB_STR "\x00\x00"
    "\x00\x00\x03\x08\x00\x1A"
    MSG_TOO_MANY_FONTLIB_LEVELS_STR "\x00\x00"
    "\x00\x00\x03\x09\x00\x2A"
    MSG_ERROR_READING_FONTLIB_DIR_STR "\x00\x00"
    "\x00\x00\x03\x0A\x00\x14"
    MSG_BAD_PK_FILE_STR "\x00\x00"
    "\x00\x00\x03\x0B\x00\x10"
    MSG_UNEXPECTED_EOF_IN_PK_STR "\x00\x00"
    "\x00\x00\x03\x0C\x00\x0E"
    MSG_EXPECTED_PRE_IN_PK_STR "\x00\x00"
    "\x00\x00\x03\x0D\x00\x0E"
    MSG_WRONG_ID_IN_PK_STR "\x00"
    "\x00\x00\x03\x0E\x00\x12"
    MSG_CHECKSUM_MISS_IN_PK_STR "\x00"
    "\x00\x00\x03\x0F\x00\x1C"
    MSG_CHAR_CODE_OUT_OF_RANGE_IN_PK_STR "\x00"
    "\x00\x00\x03\x10\x00\x18"
    MSG_PACKET_LENGTH_SMALL_IN_PK_STR "\x00"
    "\x00\x00\x03\x11\x00\x1C"
    MSG_NO_MEM_FOR_CHAR_STR "\x00\x00"
    "\x00\x00\x03\x12\x00\x16"
    MSG_UNEXPECTED_COMM_IN_PK_STR "\x00\x00"
    "\x00\x00\x03\x13\x00\x36"
    MSG_RELEASE_CHARS_STR "\x00\x00"
    "\x00\x00\x03\x14\x00\x1E"
    MSG_INTERNAL_ERROR_SPRINTF_STR "\x00"
    "\x00\x00\x03\x15\x00\x26"
    MSG_UNKNOWN_FORMAT_SPRINTF_STR "\x00"
    "\x00\x00\x03\x16\x00\x20"
    MSG_STRING_OVERFLOW_SPRINTF_STR "\x00\x00"
    "\x00\x00\x03\x17\x00\x34"
    MSG_ERROR_SEARCH_LIST_LIB_STR "\x00"
    "\x00\x00\x03\x18\x00\x2E"
    MSG_ERROR_SEARCH_LIST_PK_STR "\x00"
    "\x00\x00\x03\x19\x00\x16"
    MSG_FONT_UNDEFINED_STR "\x00"
    "\x00\x00\x03\x1A\x00\x22"
    MSG_FONT_ALREADY_DEFINED_STR "\x00\x00"
    "\x00\x00\x03\x1B\x00\x1C"
    MSG_FONT_STR_UNDEFINED_STR "\x00\x00"
    "\x00\x00\x03\x1C\x00\x20"
    MSG_RELOAD_FONT_STR "\x00"
    "\x00\x00\x03\x1D\x00\x1E"
    MSG_FONTMEM_USED_STR "\x00"
    "\x00\x00\x03\x1E\x00\x20"
    MSG_LOAD_LOADED_FONT_STR "\x00\x00"
    "\x00\x00\x03\x1F\x00\x20"
    MSG_LOAD_FONT_STR "\x00\x00"
    "\x00\x00\x03\x20\x00\x2E"
    MSG_INTERNAL_ERROR_LIB_MISMATCH_STR "\x00\x00"
    "\x00\x00\x03\x21\x00\x3E"
    MSG_LOAD_FONT_MEM_USED_STR "\x00\x00"
    "\x00\x00\x03\x22\x00\x22"
    MSG_FOUND_IN_LIB_STR "\x00"
    "\x00\x00\x03\x23\x00\x22"
    MSG_FOUND_AS_PK_STR "\x00"
    "\x00\x00\x03\x24\x00\x10"
    MSG_MEMORY_ERROR_STR "\x00"
    "\x00\x00\x03\x25\x00\x2E"
    MSG_SUBSTITUTE_FONT_STR "\x00"
    "\x00\x00\x03\x26\x00\x2C"
    MSG_FONT_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x03\x27\x00\x32"
    MSG_FONT_XY_NOT_FOUND_STR "\x00"
    "\x00\x00\x03\x28\x00\x38"
    MSG_TRY_SUBSTITUTE_STR "\x00\x00"
    "\x00\x00\x03\x29\x00\x3E"
    MSG_PREDEF_FONT_IN_LIB_STR "\x00\x00"
    "\x00\x00\x03\x2A\x00\x32"
    MSG_PREDEF_FONT_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x03\x2B\x00\x22"
    MSG_INTERNAL_ERROR_FMT_STR_STR "\x00"
    "\x00\x00\x03\x2C\x00\x28"
    MSG_BYTES_USED_FOR_CHARS_STR "\x00\x00"
    "\x00\x00\x03\x2D\x00\x34"
    MSG_SEARCHPATH_LIB_ENTRIES_NR_STR "\x00\x00"
    "\x00\x00\x03\x2E\x00\x2E"
    MSG_SEARCHPATH_PK_ENTRIES_NR_STR "\x00"
    "\x00\x00\x03\x2F\x00\x28"
    MSG_LOG_FONTDEF_START_STR "\x00"
    "\x00\x00\x03\x30\x00\x1E"
    MSG_LOG_FONTDEF_LIB_FOUND_STR "\x00"
    "\x00\x00\x03\x31\x00\x26"
    MSG_LOG_FONTDEF_LIB_DEF_STR "\x00\x00"
    "\x00\x00\x03\x32\x00\x28"
    MSG_LOG_FONTDEF_LIB_NOT_FOUND_STR "\x00"
    "\x00\x00\x03\x33\x00\x1E"
    MSG_LOG_FONTDEF_PK_FOUND_STR "\x00"
    "\x00\x00\x03\x34\x00\x24"
    MSG_LOG_FONTDEF_PK_PREDEF_STR "\x00\x00"
    "\x00\x00\x03\x35\x00\x34"
    MSG_LOG_FONTDEF_PK_PREDEF_NOT_FOUND_STR "\x00\x00"
    "\x00\x00\x03\x36\x00\x2A"
    MSG_LOG_FONTDEF_NR_BASE_PK_DIRS_PREDEF_STR "\x00\x00"
    "\x00\x00\x03\x37\x00\x24"
    MSG_LOG_FONTDEF_NO_PK_DIRS_PREDEF_STR "\x00"
    "\x00\x00\x03\x38\x00\x26"
    MSG_LOG_FONTDEF_END_STR "\x00"
    "\x00\x00\x03\x39\x00\x32"
    MSG_CANT_OPEN_FONT_CONFIG_STR "\x00"
    "\x00\x00\x03\x3A\x00\x2C"
    MSG_LOAD_FONT_CONFIG_STR "\x00\x00"
    "\x00\x00\x03\x3B\x00\x18"
    MSG_CANT_PARSE_FONT_CONFIG_LINE_STR "\x00\x00"
    "\x00\x00\x03\x3C\x00\x28"
    MSG_TOO_FEW_ARGS_STR "\x00"
    "\x00\x00\x03\x3D\x00\x1A"
    MSG_NOT_A_VALID_INT_STR "\x00"
    "\x00\x00\x03\x3E\x00\x26"
    MSG_CANT_READ_DPI_FONT_COMM_STR "\x00"
    "\x00\x00\x03\x3F\x00\x32"
    MSG_UNKNOWN_KEYWORD_IN_CONFIG_STR "\x00\x00"
    "\x00\x00\x03\x40\x00\x20"
    MSG_FONT_REMOVED_STR "\x00\x00"
    "\x00\x00\x03\x41\x00\x3E"
    MSG_FONT_REMOVED_USED_STR "\x00\x00"
    "\x00\x00\x03\x42\x00\x30"
    MSG_ERROR_IN_FONTVOLS_STR "\x00"
    "\x00\x00\x03\x43\x00\x0A"
    MSG_FONTVOLS_FIRST_DOT_STR "\x00"
    "\x00\x00\x03\x44\x00\x2E"
    MSG_COPY_FONT_TO_CACHE_STR "\x00\x00"
    "\x00\x00\x03\x45\x00\x22"
    MSG_CANT_COPY_FONT_TO_CACHE_STR "\x00"
    "\x00\x00\x03\x46\x00\x28"
    MSG_PARSE_BAD_NR_ARGS_STR "\x00\x00"
    "\x00\x00\x03\x47\x00\x24"
    MSG_PARSE_BAD_ON_OFF_STR "\x00"
    "\x00\x00\x03\x48\x00\x1C"
    MSG_PARSE_CANT_PARSE_STR "\x00\x00"
    "\x00\x00\x03\x49\x00\x28"
    MSG_PARSE_CANT_PARSE_IGNORE_STR "\x00"
    "\x00\x00\x03\x4A\x00\x2A"
    MSG_PARSE_ILLEG_ARG_STR "\x00\x00"
    "\x00\x00\x03\x4B\x00\x22"
    MSG_UNPACK_RECURSIV_STR "\x00"
    "\x00\x00\x03\x4C\x00\x30"
    MSG_UNPACK_MORE_BITS_STR "\x00\x00"
    "\x00\x00\x03\x4D\x00\x1A"
    MSG_PORT_ALREADY_EXISTS_STR "\x00"
    "\x00\x00\x03\x4E\x00\x1C"
    MSG_CANT_OPEN_PRINTER_PORT_STR "\x00"
    "\x00\x00\x03\x4F\x00\x16"
    MSG_CREATEEXTIO_FAILED_STR "\x00"
    "\x00\x00\x03\x50\x00\x24"
    MSG_YOU_PRINT_GENERIC_STR "\x00"
    "\x00\x00\x03\x51\x00\x2C"
    MSG_GENERIC_PRINT_NAME_STR "\x00\x00"
    "\x00\x00\x03\x52\x00\x34"
    MSG_GENERIC_PRINT_RESO_STR "\x00\x00"
    "\x00\x00\x03\x53\x00\x1C"
    MSG_GENERIC_PRINT_MAXD_STR "\x00"
    "\x00\x00\x03\x54\x00\x12"
    MSG_PRT_ERROR_NO_ERROR_STR "\x00\x00"
    "\x00\x00\x03\x55\x00\x12"
    MSG_PRT_ERROR_ABORT_STR "\x00"
    "\x00\x00\x03\x56\x00\x20"
    MSG_PRT_ERROR_NO_GFX_STR "\x00"
    "\x00\x00\x03\x57\x00\x1A"
    MSG_PRT_ERROR_ILLDIM_STR "\x00"
    "\x00\x00\x03\x58\x00\x22"
    MSG_PRT_ERROR_NO_MEM_VARS_STR "\x00"
    "\x00\x00\x03\x59\x00\x1C"
    MSG_PRT_ERROR_NO_MEM_BUFF_STR "\x00"
    "\x00\x00\x03\x5A\x00\x18"
    MSG_PRT_ERROR_UNKNOWN_ERR_STR "\x00"
    "\x00\x00\x03\x5B\x00\x12"
    MSG_PRT_ERR_STR "\x00"
    "\x00\x00\x03\x5C\x00\x1A"
    MSG_TRY_CLEAR_PRT_STR "\x00\x00"
    "\x00\x00\x03\x5D\x00\x0E"
    MSG_CLEAR_FAILED_STR "\x00\x00"
    "\x00\x00\x03\x5E\x00\x1E"
    MSG_GETOPT_LINEBUF_OVERFLOW_STR "\x00"
    "\x00\x00\x03\x5F\x00\x0E"
    MSG_GETOPT_ERROR_STR "\x00"
    "\x00\x00\x03\x60\x00\x22"
    MSG_GETOPT_ERROR_KEY_STR "\x00\x00"
    "\x00\x00\x03\x61\x00\x18"
    MSG_GETOPT_NO_OPTION_STRING_STR "\x00"
    "\x00\x00\x03\x62\x00\x10"
    MSG_GETOPT_MISSING_NUM_STR "\x00"
    "\x00\x00\x03\x63\x00\x16"
    MSG_GETOPT_NO_NUMBER_STR "\x00"
    "\x00\x00\x03\x64\x00\x16"
    MSG_GETOPT_MISSING_REAL_STR "\x00\x00"
    "\x00\x00\x03\x65\x00\x1C"
    MSG_GETOPT_NO_REAL_STR "\x00\x00"
    "\x00\x00\x03\x66\x00\x18"
    MSG_GETOPT_MISSING_TEX_STR "\x00\x00"
    "\x00\x00\x03\x67\x00\x1E"
    MSG_GETOPT_NO_TEX_STR "\x00\x00"
    "\x00\x00\x03\x68\x00\x18"
    MSG_GETOPT_UNKNOWN_PARAM_STR "\x00"
    "\x00\x00\x03\x69\x00\x2E"
    MSG_GETOPT_WRONG_ONOFF_PARAM_STR "\x00"
    "\x00\x00\x03\x6A\x00\x20"
    MSG_GETOPT_NO_PARAM_EXPECTED_STR "\x00\x00"
    "\x00\x00\x03\x6B\x00\x1A"
    MSG_GETOPT_UNKNOWN_KEYWORD_STR "\x00\x00"
    "\x00\x00\x03\x6C\x00\x2E"
    MSG_GETOPT_TOO_MANY_ENV_ARGS_STR "\x00"
    "\x00\x00\x03\x6D\x00\x26"
    MSG_GETOPT_NO_MEM_FOR_ENV_STR "\x00"
    "\x00\x00\x03\x6E\x00\x1C"
    MSG_GETOPT_SUPER_FILE_STR "\x00"
    "\x00\x00\x03\x6F\x00\x16"
    MSG_GETOPT_NO_INFO_STR "\x00\x00"
    "\x00\x00\x03\x70\x00\x14"
    MSG_GETOPT_PARAM_REQU_STR "\x00"
    "\x00\x00\x03\x71\x00\x12"
    MSG_GETOPT_TAB_TAB_DEF_STR "\x00\x00"
    "\x00\x00\x03\x72\x00\x14"
    MSG_GETOPT_TAB_DEF_STR "\x00"
    "\x00\x00\x03\x73\x00\x0A"
    MSG_GETOPT_NO_DEFAULT_STR "\x00\x00"
    "\x00\x00\x03\x74\x00\x20"
    MSG_GETOPT_PRESS_RET_STR "\x00\x00"
    "\x00\x00\x03\x75\x00\x14"
    MSG_SPECIAL_WAIT_FOR_PICT_STR "\x00"
    "\x00\x00\x03\x76\x00\x2E"
    MSG_SPECIAL_EXPT_REPLY_SPECIAL_STR "\x00"
    "\x00\x00\x03\x77\x00\x24"
    MSG_SPECIAL_RET_FROM_SPECIAL_STR "\x00\x00"
    "\x00\x00\x03\x78\x00\x26"
    MSG_SPECIAL_EXPT_REPLY_TPIC_STR "\x00"
    "\x00\x00\x03\x79\x00\x28"
    MSG_SPECIAL_EXPT_REPLY_BITMAP_STR "\x00"
    "\x00\x00\x03\x7A\x00\x12"
    MSG_SPECIAL_PICT_RECEIVED_STR "\x00\x00"
    "\x00\x00\x03\x7B\x00\x1C"
    MSG_SPECIAL_FOUND_NO_SPECIAL_STR "\x00"
    "\x00\x00\x03\x7C\x00\x24"
    MSG_SPECIAL_CANT_CREATE_PORT_STR "\x00\x00"
    "\x00\x00\x03\x7D\x00\x18"
    MSG_NO_DVI_FILENAME_STR "\x00\x00"
    "\x00\x00\x03\x7E\x00\x1A"
    MSG_CANT_OPEN_DVI_FILE_STR "\x00"
    "\x00\x00\x03\x7F\x00\x34"
    MSG_INCOMPLETE_DVI_REVERSE_STR "\x00\x00"
    "\x00\x00\x03\x80\x00\x16"
    MSG_INCOMPLETE_DVI_STR "\x00\x00"
    "\x00\x00\x03\x81\x00\x1C"
    MSG_UNDEFINED_DVI_COMMAND_STR "\x00\x00"
    "\x00\x00\x03\x82\x00\x24"
    MSG_NO_LAND_WITH_GENERIC_STR "\x00\x00"
    "\x00\x00\x03\x83\x00\x3A"
    MSG_NO_LAND_XDPI_YDPI_STR "\x00"
    "\x00\x00\x03\x84\x00\x10"
    MSG_PRINTER_NAME_STR "\x00\x00"
    "\x00\x00\x03\x85\x00\x10"
    MSG_PRINTER_ID_STR "\x00\x00"
    "\x00\x00\x03\x86\x00\x18"
    MSG_MISSING_KEYWORD_STR "\x00\x00"
    "\x00\x00\x03\x87\x00\x32"
    MSG_INCOMPLETE_PRT_DESC_STR "\x00\x00"
    "\x00\x00\x03\x88\x00\x28"
    MSG_USE_DRAFT_OPT_STR "\x00\x00"
    "\x00\x00\x03\x89\x00\x18"
    MSG_PRT_RESO_STR "\x00\x00"
    "\x00\x00\x03\x8A\x00\x24"
    MSG_PRT_WIDTH_STR "\x00"
    "\x00\x00\x03\x8B\x00\x28"
    MSG_PRT_PASSES_STR "\x00\x00"
    "\x00\x00\x03\x8C\x00\x14"
    MSG_PRT_BITMAP_HEIGHT_STR "\x00"
    "\x00\x00\x03\x8D\x00\x1A"
    MSG_PRT_DEF_BUF_SIZE_STR "\x00\x00"
    "\x00\x00\x03\x8E\x00\x2A"
    MSG_PRT_MOVE_TO_POINT_STR "\x00"
    "\x00\x00\x03\x8F\x00\x32"
    MSG_PRT_ONE_GRAPHIC_COMMAND_STR "\x00\x00"
    "\x00\x00\x03\x90\x00\x26"
    MSG_PRT_SKIP_WITH_SPACES_STR "\x00\x00"
    "\x00\x00\x03\x91\x00\x26"
    MSG_PRT_BAD_RESO_STR "\x00\x00"
    "\x00\x00\x03\x92\x00\x20"
    MSG_PRT_USE_RES_STR "\x00"
    "\x00\x00\x03\x93\x00\x36"
    MSG_UNKNOWN_BLANKING_STR "\x00"
    "\x00\x00\x03\x94\x00\x34"
    MSG_ILLEG_PARM_FOR_KEY_STR "\x00"
    "\x00\x00\x03\x95\x00\x30"
    MSG_GROUPING_RANGE_STR "\x00\x00"
    "\x00\x00\x03\x96\x00\x2C"
    MSG_CANT_OPEN_PRT_CONFIG_STR "\x00"
    "\x00\x00\x03\x97\x00\x32"
    MSG_READ_PRT_CONFIG_STR "\x00"
    "\x00\x00\x03\x98\x00\x0C"
    MSG_THATS_ALL_STR "\x00"
    "\x00\x00\x03\x99\x00\x1A"
    MSG_AVAILABLE_PRINTERS_STR "\x00"
    "\x00\x00\x03\x9A\x00\x30"
    MSG_MAX_GROUPING_STR "\x00"
    "\x00\x00\x03\x9B\x00\x14"
    MSG_PRINT_WITH_STR "\x00\x00"
    "\x00\x00\x03\x9C\x00\x24"
    MSG_UNKNOWN_PRT_CONFIG_STR "\x00\x00"
    "\x00\x00\x03\x9D\x00\x30"
    MSG_PRT_NO_OUTPUT_REDIRECTION_STR "\x00\x00"
    "\x00\x00\x03\x9E\x00\x1E"
    MSG_CANT_OPEN_OUTPUT_FILE_STR "\x00\x00"
    "\x00\x00\x03\x9F\x00\x1C"
    MSG_NO_PRT_TYPE_FLAG_STR "\x00\x00"
    "\x00\x00\x03\xA0\x00\x2E"
    MSG_KEY_TWICE_STR "\x00\x00"
    "\x00\x00\x03\xA1\x00\x2C"
    MSG_KEY_CONTEXT_STR "\x00"
    "\x00\x00\x03\xA2\x00\x1C"
    MSG_CANT_WRITE_OUTPUT_FILE_STR "\x00"
    "\x00\x00\x03\xA3\x00\x20"
    MSG_WRONG_NUM_COPIES_STR "\x00"
    "\x00\x00\x03\xA4\x00\x2A"
    MSG_PRINT_AT_LEAST_ONE_PAGE_STR "\x00\x00"
    "\x00\x00\x03\xA5\x00\x3E"
    MSG_WRONG_RESO_FORMAT_STR "\x00\x00"
    "\x00\x00\x03\xA6\x00\x2E"
    MSG_NO_ODD_NO_EVEN_STR "\x00"
    "\x00\x00\x03\xA7\x00\x26"
    MSG_DENSITY_RANGE_STR "\x00\x00"
    "\x00\x00\x03\xA8\x00\x2C"
    MSG_BITMAP_MEM_MINIMUM_STR "\x00\x00"
    "\x00\x00\x03\xA9\x00\x42"
    MSG_TRY_HELP_SHOWP_STR "\x00\x00"
    "\x00\x00\x03\xAA\x00\x1A"
    MSG_UNKNOWN_PAPER_SIZE_STR "\x00\x00"
    "\x00\x00\x03\xAB\x00\x32"
    MSG_REVERSE_TWOUP_ERR_STR "\x00\x00"
    "\x00\x00\x03\xAC\x00\x26"
    MSG_PAPER_WIDTH_HEIGHT_ERR_STR "\x00\x00"
    "\x00\x00\x03\xAD\x00\x18"
    MSG_OPTIONS_HELP_STR "\x00\x00"
    "\x00\x00\x03\xAE\x00\x20"
    MSG_OPTIONS_GUI_STR "\x00\x00"
    "\x00\x00\x03\xAF\x00\x18"
    MSG_OPTIONS_FONTDIR_STR "\x00"
    "\x00\x00\x03\xB0\x00\x18"
    MSG_OPTIONS_FONTMEM_STR "\x00\x00"
    "\x00\x00\x03\xB1\x00\x18"
    MSG_OPTIONS_MAXBITMEM_STR "\x00\x00"
    "\x00\x00\x03\xB2\x00\x16"
    MSG_OPTIONS_MAXBITMEM2_STR "\x00"
    "\x00\x00\x03\xB3\x00\x1C"
    MSG_OPTIONS_PRTBUFFER_STR "\x00\x00"
    "\x00\x00\x03\xB4\x00\x0E"
    MSG_OPTIONS_FROM_STR "\x00"
    "\x00\x00\x03\xB5\x00\x0E"
    MSG_OPTIONS_TO_STR "\x00\x00"
    "\x00\x00\x03\xB6\x00\x1A"
    MSG_OPTIONS_NUMBER_STR "\x00\x00"
    "\x00\x00\x03\xB7\x00\x1C"
    MSG_OPTIONS_ODD_STR "\x00\x00"
    "\x00\x00\x03\xB8\x00\x1C"
    MSG_OPTIONS_EVEN_STR "\x00"
    "\x00\x00\x03\xB9\x00\x26"
    MSG_OPTIONS_PHYSICAL_STR "\x00\x00"
    "\x00\x00\x03\xBA\x00\x24"
    MSG_OPTIONS_HOFFSET_STR "\x00"
    "\x00\x00\x03\xBB\x00\x2E"
    MSG_OPTIONS_HVOFFSET2_STR "\x00\x00"
    "\x00\x00\x03\xBC\x00\x22"
    MSG_OPTIONS_VOFFSET_STR "\x00"
    "\x00\x00\x03\xBD\x00\x0E"
    MSG_OPTIONS_PRINTER_STR "\x00\x00"
    "\x00\x00\x03\xBE\x00\x1A"
    MSG_OPTIONS_OPTIMIZE_STR "\x00\x00"
    "\x00\x00\x03\xBF\x00\x16"
    MSG_OPTIONS_DRAFT_STR "\x00\x00"
    "\x00\x00\x03\xC0\x00\x10"
    MSG_OPTIONS_DENSITY_STR "\x00"
    "\x00\x00\x03\xC1\x00\x22"
    MSG_OPTIONS_DENSITY2_STR "\x00\x00"
    "\x00\x00\x03\xC2\x00\x16"
    MSG_OPTIONS_UNIDIRECT_STR "\x00\x00"
    "\x00\x00\x03\xC3\x00\x24"
    MSG_OPTIONS_COPIES_STR "\x00\x00"
    "\x00\x00\x03\xC4\x00\x1A"
    MSG_OPTIONS_LANDSCAPE_STR "\x00\x00"
    "\x00\x00\x03\xC5\x00\x24"
    MSG_OPTIONS_TWOPAGE_STR "\x00\x00"
    "\x00\x00\x03\xC6\x00\x2C"
    MSG_OPTIONS_MOFFSET_STR "\x00\x00"
    "\x00\x00\x03\xC7\x00\x30"
    MSG_OPTIONS_BOOK_STR "\x00"
    "\x00\x00\x03\xC8\x00\x1A"
    MSG_OPTIONS_IFF_STR "\x00\x00"
    "\x00\x00\x03\xC9\x00\x24"
    MSG_OPTIONS_SKIPFORMFEED_STR "\x00"
    "\x00\x00\x03\xCA\x00\x18"
    MSG_OPTIONS_REVERSE_STR "\x00\x00"
    "\x00\x00\x03\xCB\x00\x12"
    MSG_OPTIONS_RESOLUTION_STR "\x00"
    "\x00\x00\x03\xCC\x00\x32"
    MSG_OPTIONS_RESOLUTION2_STR "\x00\x00"
    "\x00\x00\x03\xCD\x00\x22"
    MSG_OPTIONS_WIDTH_STR "\x00\x00"
    "\x00\x00\x03\xCE\x00\x22"
    MSG_OPTIONS_HEIGHT_STR "\x00"
    "\x00\x00\x03\xCF\x00\x12"
    MSG_OPTIONS_PRELOAD_STR "\x00"
    "\x00\x00\x03\xD0\x00\x0A"
    MSG_OPTIONS_FAST_STR "\x00"
    "\x00\x00\x03\xD1\x00\x26"
    MSG_OPTIONS_FAST2_STR "\x00"
    "\x00\x00\x03\xD2\x00\x20"
    MSG_OPTIONS_MARK_STR "\x00\x00"
    "\x00\x00\x03\xD3\x00\x1C"
    MSG_OPTIONS_STATISTIC_STR "\x00\x00"
    "\x00\x00\x03\xD4\x00\x20"
    MSG_OPTIONS_DEBUGSTAT_STR "\x00\x00"
    "\x00\x00\x03\xD5\x00\x16"
    MSG_OPTIONS_NOLOG_STR "\x00\x00"
    "\x00\x00\x03\xD6\x00\x20"
    MSG_OPTIONS_OUTTO_STR "\x00\x00"
    "\x00\x00\x03\xD7\x00\x36"
    MSG_OPTIONS_OUTTO2_STR "\x00"
    "\x00\x00\x03\xD8\x00\x0E"
    MSG_OPTIONS_LOGNAME_STR "\x00\x00"
    "\x00\x00\x03\xD9\x00\x1A"
    MSG_OPTIONS_SHOWPRINTERS_STR "\x00\x00"
    "\x00\x00\x03\xDA\x00\x14"
    MSG_OPTIONS_ACCOUNTING_STR "\x00\x00"
    "\x00\x00\x03\xDB\x00\x16"
    MSG_OPTIONS_PRIORITY_STR "\x00"
    "\x00\x00\x03\xDC\x00\x30"
    MSG_OPTIONS_SPECIALHOST_STR "\x00\x00"
    "\x00\x00\x03\xDD\x00\x12"
    MSG_OPTIONS_PRINTAUTHOR_STR "\x00\x00"
    "\x00\x00\x03\xDE\x00\x0A"
    MSG_OPTIONS_DVIFILE_STR "\x00\x00"
    "\x00\x00\x03\xDF\x00\x12"
    MSG_OPTIONS_PUBSCREEN_STR "\x00"
    "\x00\x00\x03\xE0\x00\x18"
    MSG_OPTIONS_PAPER_STR "\x00"
    "\x00\x00\x03\xE1\x00\x26"
    MSG_OPTIONS_PAPER2_STR "\x00\x00"
    "\x00\x00\x03\xE2\x00\x1E"
    MSG_MUI_MW_DESCRIPTION_STR "\x00"
    "\x00\x00\x03\xE3\x00\x12"
    MSG_MUI_MW_TITEL_STR "\x00\x00"
    "\x00\x00\x03\xE4\x00\x0C"
    MSG_MUI_MW_DVIFILE_STR "\x00\x00"
    "\x00\x00\x03\xE5\x00\x12"
    MSG_MUI_MW_ASL_DVIFILE_STR "\x00"
    "\x00\x00\x03\xE6\x00\x10"
    MSG_MUI_MW_SEITEN_STR "\x00\x00"
    "\x00\x00\x03\xE7\x00\x08"
    MSG_MUI_MW_VON_STR "\x00"
    "\x00\x00\x03\xE8\x00\x06"
    MSG_MUI_MW_BIS_STR "\x00"
    "\x00\x00\x03\xE9\x00\x1C"
    MSG_MUI_MW_NUM_STR "\x00"
    "\x00\x00\x03\xEA\x00\x0A"
    MSG_MUI_MW_KOPIEN_STR "\x00"
    "\x00\x00\x03\xEB\x00\x08"
    MSG_MUI_MW_DRUCKEN_STR "\x00\x00"
    "\x00\x00\x03\xEC\x00\x08"
    MSG_MUI_MW_PREF_STR "\x00\x00"
    "\x00\x00\x03\xED\x00\x08"
    MSG_MUI_MW_CANCEL_STR "\x00"
    "\x00\x00\x03\xEE\x00\x1C"
    MSG_MUI_MW_APP_FAIL_STR "\x00"
    "\x00\x00\x03\xEF\x00\x04"
    MSG_MUI_MW_SEITEN_ALL_STR "\x00"
    "\x00\x00\x03\xF0\x00\x08"
    MSG_MUI_MW_SEITEN_VONBIS_STR "\x00"
    "\x00\x00\x03\xF1\x00\x12"
    MSG_MUI_ME_WINTITLE_STR "\x00"
    "\x00\x00\x03\xF2\x00\x08"
    MSG_MUI_ME_CLOSE_STR "\x00\x00"
    "\x00\x00\x03\xF3\x00\x0A"
    MSG_MUI_ME_TITEL_STR "\x00\x00"
    "\x00\x00\x03\xF4\x00\x10"
    MSG_MUI_WO_WINTITLE_STR "\x00\x00"
    "\x00\x00\x03\xF5\x00\x06"
    MSG_MUI_WO_TITEL_STR "\x00"
    "\x00\x00\x03\xF6\x00\x0A"
    MSG_MUI_WO_SEEMESS_STR "\x00"
    "\x00\x00\x03\xF7\x00\x0A"
    MSG_MUI_WORK_DOALL_STR "\x00"
    "\x00\x00\x03\xF8\x00\x0C"
    MSG_MUI_WORK_START_STR "\x00\x00"
    "\x00\x00\x03\xF9\x00\x0A"
    MSG_MUI_WORK_END_STR "\x00"
    "\x00\x00\x03\xFA\x00\x06"
    MSG_MUI_WORK_FROM_STR "\x00\x00"
    "\x00\x00\x03\xFB\x00\x06"
    MSG_MUI_WORK_PAGE_STR "\x00\x00"
    "\x00\x00\x03\xFC\x00\x04"
    MSG_MUI_WORK_TO_STR "\x00\x00"
    "\x00\x00\x03\xFD\x00\x0C"
    MSG_MUI_FA_WINTITLE_STR "\x00"
    "\x00\x00\x03\xFE\x00\x0E"
    MSG_MUI_FA_TITEL_STR "\x00\x00"
};

#endif /* CATCOMP_BLOCK */


/****************************************************************************/


struct LocaleInfo
{
    APTR li_LocaleBase;
    APTR li_Catalog;
};


#ifdef CATCOMP_CODE

STRPTR GetString(struct LocaleInfo *li, LONG stringNum)
{
LONG   *l;
UWORD  *w;
STRPTR  builtIn;

    l = (LONG *)CatCompBlock;

    while (*l != stringNum)
    {
        w = (UWORD *)((ULONG)l + 4);
        l = (LONG *)((ULONG)l + (ULONG)*w + 6);
    }
    builtIn = (STRPTR)((ULONG)l + 6);

#define XLocaleBase LocaleBase
#define LocaleBase li->li_LocaleBase
    
    if (LocaleBase)
        return(GetCatalogStr(li->li_Catalog,stringNum,builtIn));
#define LocaleBase XLocaleBase
#undef XLocaleBase

    return(builtIn);
}


#endif /* CATCOMP_CODE */


/****************************************************************************/


#endif /* LOCALSTR_H */
