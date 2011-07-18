#ifndef CHARM_HEADER
#define CHARM_HEADER

#define CHARM_VERSION "0.0.1"

// int pos_x;
// int pos_y;

int get_width();
int get_height();

void cursor_off();
void cursor_on();

void echo_off();
void echo_on();

void raw_on();
void raw_off();

void blocking_off();
void blocking_on();

int get_x();
int get_y();
void move_cursor(int x, int y);
void blot_char(char c);
void blot_string(char *s);
void hcenter_string(char *s);
void vcenter_string(char *s);

void clear_screen();

void handle_signal(int signal);
void start_charm();
void end_charm();

typedef enum {
	KEY_BACKSPACE,
	KEY_TAB,
	KEY_NEWLINE,

	KEY_SPACE,
	KEY_EXCLAMATION,
	KEY_DOUBLE_QUOTE,
	KEY_HASH,
	KEY_DOLLAR,
	KEY_PERCENT,
	KEY_AMPERSAND,
	KEY_SINGLE_QUOTE,
	KEY_LEFT_PAREN,
	KEY_RIGHT_PAREN,
	KEY_ASTERISK,
	KEY_PLUS,
	KEY_COMMA,
	KEY_MINUS,
	KEY_PERIOD,
	KEY_SLASH,

	KEY_0,
	KEY_1,
	KEY_2,
	KEY_3,
	KEY_4,
	KEY_5,
	KEY_6,
	KEY_7,
	KEY_8,
	KEY_9,

	KEY_COLON,
	KEY_SEMICOLON,
	KEY_LESS_THAN,
	KEY_EQUALS,
	KEY_GREATER_THAN,
	KEY_QUESTION,
	KEY_AT,

	KEY_CAPITAL_A,
	KEY_CAPITAL_B,
	KEY_CAPITAL_C,
	KEY_CAPITAL_D,
	KEY_CAPITAL_E,
	KEY_CAPITAL_F,
	KEY_CAPITAL_G,
	KEY_CAPITAL_H,
	KEY_CAPITAL_I,
	KEY_CAPITAL_J,
	KEY_CAPITAL_K,
	KEY_CAPITAL_L,
	KEY_CAPITAL_M,
	KEY_CAPITAL_N,
	KEY_CAPITAL_O,
	KEY_CAPITAL_P,
	KEY_CAPITAL_Q,
	KEY_CAPITAL_R,
	KEY_CAPITAL_S,
	KEY_CAPITAL_T,
	KEY_CAPITAL_U,
	KEY_CAPITAL_V,
	KEY_CAPITAL_W,
	KEY_CAPITAL_X,
	KEY_CAPITAL_Y,
	KEY_CAPITAL_Z,

	KEY_LEFT_BRACKET,
	KEY_BACKSLASH,
	KEY_RIGHT_BRACKET,
	KEY_CARET,
	KEY_UNDERSCORE,
	KEY_BACKTICK,

	KEY_A,
	KEY_B,
	KEY_C,
	KEY_D,
	KEY_E,
	KEY_F,
	KEY_G,
	KEY_H,
	KEY_I,
	KEY_J,
	KEY_K,
	KEY_L,
	KEY_M,
	KEY_N,
	KEY_O,
	KEY_P,
	KEY_Q,
	KEY_R,
	KEY_S,
	KEY_T,
	KEY_U,
	KEY_V,
	KEY_W,
	KEY_X,
	KEY_Y,
	KEY_Z,

	KEY_LEFT_BRACE,
	KEY_PIPE,
	KEY_RIGHT_BRACE,
	KEY_TILDE,

	KEY_UP,
	KEY_DOWN,
	KEY_RIGHT,
	KEY_LEFT,

	KEY_ESCAPE,

	KEY_UNKNOWN
} key;

key parse_key(char *buf);
key get_key();

#endif