// Copyright 2021 david@impstyle.com (@zwnk)
// SPDX-License-Identifier: GPL-2.0-or-later

// https://github.com/qmk/qmk_firmware/blob/master/docs/keycodes.md

#include QMK_KEYBOARD_H

enum custom_layers {
    _QWERTY,
    _LOWER,
    _RAISE,
};

#define RAISE LT(_RAISE, KC_COPY)
#define LOWER LT(_LOWER, KC_PASTE)
#define SPC_C LCTL_T(KC_SPC)
// #define SPC_S LSFT_T(KC_SPC)
#define SPC_L LGUI_T(KC_SPC)
#define SPC_R RGUI_T(KC_SPC)
#define MYCM_C LCTL_T(KC_MYCM)

// not working:
// KC_ERAS
// KC_CNCL

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [_QWERTY] = LAYOUT_6x6(
        KC_F1  , KC_F2 , KC_F3 , KC_F4 , KC_F5 , KC_F6 ,                         KC_F7 , KC_F8 , KC_F9 ,KC_F10 ,KC_F11 ,KC_F12 ,
        KC_ESC , KC_1  , KC_2  , KC_3  , KC_4  , KC_5  ,                         KC_6  , KC_7  , KC_8  , KC_9  , KC_0  ,KC_BSPC,
        KC_TAB , KC_Q  , KC_W  , KC_E  , KC_R  , KC_T  ,                         KC_Y  , KC_U  , KC_I  , KC_O  , KC_P  ,KC_MINS,
        KC_LSFT, KC_A  , KC_S  , KC_D  , KC_F  , KC_G  ,                         KC_H  , KC_J  , KC_K  , KC_L  ,KC_SCLN,SFT_T(KC_QUOT),
        KC_LCTL, KC_Z  , KC_X  , KC_C  , KC_V  , KC_B  ,                         KC_N  , KC_M  ,KC_COMM,KC_DOT ,KC_LBRC,CTL_T(KC_BACKSLASH),
                         KC_NUBS,RCTL_T(KC_INS),                                                KC_RBRC, KC_SLSH,
                                         RAISE , SPC_C,                          KC_SPC,  LOWER,
                                         LALT_T(KC_WBAK), SPC_L,                  SPC_R, RALT_T(KC_WFWD),
                                         KC_BSPC, LALT_T(KC_TAB),        RALT_T(KC_ENT), KC_ESC
    ),

    [_LOWER] = LAYOUT_6x6(
        KC_F13 , KC_F14, KC_F15, KC_F16, KC_F17, KC_F18,                        KC_F19 , KC_F20, KC_F21,KC_F22 ,KC_F23 ,KC_F24,
        KC_GRV, KC_WFAV,KC_MRWD,KC_MFFD,KC_WREF,KC_MSEL,                        XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,KC_PAST,KC_BRIU,
        XXXXXXX,KC_CALC,KC_INT1,KC_INT2,KC_INT3,KC_INT4,                        KC_WSTP, KC_P7 , KC_P8 , KC_P9 ,KC_PMNS,KC_BRID,
        _______,KC_HOME,KC_PGUP,KC_PGDN,KC_END ,KC_APP,                         KC_SLCT, KC_P4 , KC_P5 , KC_P6 ,KC_PPLS,_______,
        _______,KC_WREF,KC_WBAK,KC_WFWD,KC_WHOM,KC_WSCH,                        KC_EXEC, KC_P1 , KC_P2 , KC_P3 ,KC_PENT,_______,
                        KC_PAUS,KC_PSCR,                                                         KC_P0 ,KC_PDOT,
                                               KC_FIND,MYCM_C,             KC_MYCM,XXXXXXX,
                                               _______,_______,            _______,XXXXXXX,
                                               KC_DEL ,_______,            _______,XXXXXXX
    ),

    [_RAISE] = LAYOUT_6x6(
        KC_F13 , KC_F14, KC_F15, KC_F16, KC_F17, KC_F18,                        KC_F19 , KC_F20, KC_F21,KC_F22 ,KC_F23 ,KC_F24,
        QK_BOOT,XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,                        KC_LNG1,XXXXXXX,KC_NUM ,XXXXXXX,KC_SCRL,KC_MUTE,
        KC_WAKE,XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,XXXXXXX,                        KC_LNG2,KC_MPRV,KC_MPLY,KC_MNXT,XXXXXXX,KC_VOLU,
        _______,KC_LEFT,KC_UP  ,KC_DOWN,KC_RGHT,KC_HELP,                        KC_LNG3,XXXXXXX,KC_MS_UP,XXXXXXX,XXXXXXX,KC_VOLD,
        _______,XXXXXXX,KC_CUT ,KC_UNDO,KC_AGIN,KC_STOP,                        KC_LNG4,KC_MS_LEFT,KC_MS_DOWN,KC_MS_RIGHT,XXXXXXX,XXXXXXX,
                        XXXXXXX,XXXXXXX,                                                        KC_EQL ,KC_MAIL,
                                                XXXXXXX,_______,         KC_MS_BTN1,KC_MS_BTN2,
                                                XXXXXXX,_______,            _______,KC_MS_BTN3,
                                                KC_SLEP,_______,         KC_MS_BTN5,KC_MS_BTN4
    )
};
