#include "charm.h"
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ANSI Escape Codes
// http://en.wikipedia.org/wiki/ANSI_escape_code

// Terminal information
// http://developer.apple.com/library/mac/#documentation/Darwin/Reference/ManPages/man4/tty.4.html

// Disabling echo
// http://www.gnu.org/s/hello/manual/libc/getpass.html

int pos_x;
int pos_y;

int get_width() {
	struct winsize ws;
	(void) ioctl(0, TIOCGWINSZ, &ws);

	return (int) ws.ws_col;
}

int get_height() {
	struct winsize ws;
	(void) ioctl(0, TIOCGWINSZ, &ws);

	return (int) ws.ws_row;
}

void cursor_off() {
	printf("\033[?25l");
	(void) fflush(stdout);
}

void cursor_on() {
	printf("\033[?25h");
	(void) fflush(stdout);
}

void echo_off() {
	struct termios term;
	(void) tcgetattr(fileno(stdout), &term);
	term.c_lflag &= ~ECHO;
	(void) tcsetattr(fileno(stdout), TCSAFLUSH, &term);
}

void echo_on() {
	struct termios term;
	(void) tcgetattr(fileno(stdout), &term);
	term.c_lflag |= ECHO;
	(void) tcsetattr(fileno(stdout), TCSAFLUSH, &term);
}

void raw_on() {
	struct termios term;
	(void) tcgetattr(fileno(stdout), &term);
	term.c_lflag &= ~ICANON;
	(void) tcsetattr(fileno(stdout), TCSAFLUSH, &term);
}

void raw_off() {
	struct termios term;
	(void) tcgetattr(fileno(stdout), &term);
	term.c_lflag |= ICANON;
	(void) tcsetattr(fileno(stdout), TCSAFLUSH, &term);
}

void blocking_off() {
	struct termios term;
	(void) tcgetattr(fileno(stdout), &term);
	term.c_cc[VMIN] = (cc_t) 0;
	(void) tcsetattr(fileno(stdout), TCSAFLUSH, &term);
}

void blocking_on() {
	struct termios term;
	(void) tcgetattr(fileno(stdout), &term);
	term.c_cc[VMIN] = (cc_t) 1;
	(void) tcsetattr(fileno(stdout), TCSAFLUSH, &term);
}

int get_x() {
	return pos_x;
}

int get_y() {
	return pos_y;
}

// (0, 0) = top left
// (width - 1, 0) = top right
// (0, height - 1) = bottom left
// (width - 1, height - 1) = bottom right
void move_cursor(int x, int y) {
	pos_x = x;
	pos_y = y;
	printf("\033[%d;%dH", y + 1, x + 1);
	(void) fflush(stdout);
}

void blot_char(char c) {
	if (c == '\n') {
		pos_y++;
		move_cursor(0, pos_y);
	}
	else {
		(void) putchar(c);
		(void) fflush(stdout);
		pos_x++;
	}
}

void blot_string(char *s) {
	int i;
	for (i = 0; i < (int) strlen(s); i++) {
		blot_char(s[i]);
	}
}

void hcenter_string(char *s) {
	move_cursor((get_width() - (int) strlen(s))/2, pos_y);
	blot_string(s);
}

void vcenter_string(char *s) {
	move_cursor((get_width() - (int) strlen(s))/2 - 1, get_height()/2 - 1);
	blot_string(s);
}

void clear_screen() {
	printf("\033[2J");
	(void) fflush(stdout);
}

void handle_signal(int signal) {
	end_charm();
	exit(0);
}

void start_charm() {
	(void) signal(SIGINT, handle_signal);

	cursor_off();
	echo_off();
	raw_on();
	blocking_off();
	move_cursor(0, 0);
	clear_screen();
}

void end_charm() {
	move_cursor(0, 0);
	clear_screen();
	blocking_on();
	raw_off();
	echo_on();
	cursor_on();
}

key parse_key(char *buf) {
	switch(buf[0]) {
		case '\x7f':
		case '\b': return KEY_BACKSPACE;
		case '\t': return KEY_TAB;
		case '\n': return KEY_NEWLINE;

		case ' ': return KEY_SPACE;
		case '!': return KEY_EXCLAMATION;
		case '\"': return KEY_DOUBLE_QUOTE;
		case '#': return KEY_HASH;
		case '$': return KEY_DOLLAR;
		case '%': return KEY_PERCENT;
		case '&': return KEY_AMPERSAND;
		case '\'': return KEY_SINGLE_QUOTE;
		case '(': return KEY_LEFT_PAREN;
		case ')': return KEY_RIGHT_PAREN;
		case '*': return KEY_ASTERISK;
		case '+': return KEY_PLUS;
		case ',': return KEY_COMMA;
		case '-': return KEY_MINUS;
		case '.': return KEY_PERIOD;
		case '/': return KEY_SLASH;

		case '0': return KEY_0;
		case '1': return KEY_1;
		case '2': return KEY_2;
		case '3': return KEY_3;
		case '4': return KEY_4;
		case '5': return KEY_5;
		case '6': return KEY_6;
		case '7': return KEY_7;
		case '8': return KEY_8;
		case '9': return KEY_9;

		case ':': return KEY_COLON;
		case ';': return KEY_SEMICOLON;
		case '<': return KEY_LESS_THAN;
		case '=': return KEY_EQUALS;
		case '>': return KEY_GREATER_THAN;
		case '?': return KEY_QUESTION;
		case '@': return KEY_AT;

		case 'A': return KEY_CAPITAL_A;
		case 'B': return KEY_CAPITAL_B;
		case 'C': return KEY_CAPITAL_C;
		case 'D': return KEY_CAPITAL_D;
		case 'E': return KEY_CAPITAL_E;
		case 'F': return KEY_CAPITAL_F;
		case 'G': return KEY_CAPITAL_G;
		case 'H': return KEY_CAPITAL_H;
		case 'I': return KEY_CAPITAL_I;
		case 'J': return KEY_CAPITAL_J;
		case 'K': return KEY_CAPITAL_K;
		case 'L': return KEY_CAPITAL_L;
		case 'M': return KEY_CAPITAL_M;
		case 'N': return KEY_CAPITAL_N;
		case 'O': return KEY_CAPITAL_O;
		case 'P': return KEY_CAPITAL_P;
		case 'Q': return KEY_CAPITAL_Q;
		case 'R': return KEY_CAPITAL_R;
		case 'S': return KEY_CAPITAL_S;
		case 'T': return KEY_CAPITAL_T;
		case 'U': return KEY_CAPITAL_U;
		case 'V': return KEY_CAPITAL_V;
		case 'W': return KEY_CAPITAL_W;
		case 'X': return KEY_CAPITAL_X;
		case 'Y': return KEY_CAPITAL_Y;
		case 'Z': return KEY_CAPITAL_Z;

		case '[': return KEY_LEFT_BRACKET;
		case '\\': return KEY_BACKSLASH;
		case ']': return KEY_RIGHT_BRACKET;
		case '^': return KEY_CARET;
		case '_': return KEY_UNDERSCORE;
		case '`': return KEY_BACKTICK;

		case 'a': return KEY_A;
		case 'b': return KEY_B;
		case 'c': return KEY_C;
		case 'd': return KEY_D;
		case 'e': return KEY_E;
		case 'f': return KEY_F;
		case 'g': return KEY_G;
		case 'h': return KEY_H;
		case 'i': return KEY_I;
		case 'j': return KEY_J;
		case 'k': return KEY_K;
		case 'l': return KEY_L;
		case 'm': return KEY_M;
		case 'n': return KEY_N;
		case 'o': return KEY_O;
		case 'p': return KEY_P;
		case 'q': return KEY_Q;
		case 'r': return KEY_R;
		case 's': return KEY_S;
		case 't': return KEY_T;
		case 'u': return KEY_U;
		case 'v': return KEY_V;
		case 'w': return KEY_W;
		case 'x': return KEY_X;
		case 'y': return KEY_Y;
		case 'z': return KEY_Z;

		case '{': return KEY_LEFT_BRACE;
		case '|': return KEY_PIPE;
		case '}': return KEY_RIGHT_BRACE;
		case '~': return KEY_TILDE;

		case '\x1b':
			switch(buf[1]) {
				case '\0': return KEY_ESCAPE;
				case '[':
					switch(buf[2]) {
						case 'A': return KEY_UP;
						case 'B': return KEY_DOWN;
						case 'C': return KEY_RIGHT;
						case 'D': return KEY_LEFT;
					}
			}

		default: return KEY_UNKNOWN;
	}
}

key get_key() {
	int i;

	char *buf = (char *) malloc(3 * sizeof(char));

	if (buf == NULL) {
		return KEY_UNKNOWN;
	}

	buf[0] = '\0';
	buf[1] = '\0';
	buf[2] = '\0';

	i = 0;

	char c;

	int n = 0;

	// Read at least one character.
	while (n < 1) {
		n = read(fileno(stdin), &c, 1);
	}

	while (i < 3 && n > 0) {
		buf[i++] = c;
		n = read(fileno(stdin), &c, 1);
	}

	key k = parse_key(buf);

	free(buf);

	return k;
}