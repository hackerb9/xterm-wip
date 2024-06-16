/* $XTermId: graphics_sixel.c,v 1.57 2024/05/16 20:03:50 tom Exp $ */

/*
 * Copyright 2014-2023,2024 by Thomas E. Dickey
 * Copyright 2024           by Benjamin A. Wong
 * Copyright 2014-2022,2023 by Ross Combs
 *
 *                         All Rights Reserved
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE ABOVE LISTED COPYRIGHT HOLDER(S) BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name(s) of the above copyright
 * holders shall not be used in advertising or otherwise to promote the
 * sale, use or other dealings in this Software without prior written
 * authorization.
 */

#include <xterm.h>

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include <data.h>
#include <VTparse.h>
#include <ptyx.h>

#include <assert.h>
#include <graphics.h>
#include <graphics_sixel.h>

/***====================================================================***/

#if (__STDC_VERSION__ >= 202311L || defined(__GNUC__))
#define SIXEL_PRINTF "%06b"
#else
#define SIXEL_PRINTF "0x%X"
#endif

typedef struct {
    RegisterNum current_register;
    RegisterNum background;	/* current background color register or hole */
    int aspect_vertical;	/* sixel aspect ratio (numerator)  */
    int aspect_horizontal;	/* sixel aspect ratio (denominator)  */
    int declared_width;		/* size (in screen pixels) from DECGRA */
    int declared_height;	/* size (in screen pixels) from DECGRA */
    int row;			/* context used during parsing */
    int col;			/* context used during parsing */
} SixelContext;

/* Saved state for the parse_sixel_char() state machine  */
static XtermWidget s_xw;
static TScreen *s_screen;
static Graphic *s_graphic;
static SixelContext s_context;
static int s_prev_row;		/* Dirty graphic cursor last location for screen updating */
static int s_prev_col;
static int s_accumulator;	/* Accumulation of decimal digits. (-1 for no digits, use default) */
static Boolean s_repeating;	/* true if gathering digits for '!' (repeat) */

/*
 * States for gathering params for '#' (color)
 */
static enum {
    s_NOTCOLORING
    ,s_GETTINGREGISTER
    ,s_GETTINGCOLORSPACE
    ,s_GETTINGPC1
    ,s_GETTINGPC2
    ,s_GETTINGPC3
    ,s_COLORINGDONE
} s_color_state;

static int s_color_params[s_COLORINGDONE + 1];
static RegisterNum s_Pregister;

/*
 * States for gathering params from DECGRA ('"' set raster attributes)
 * Refer to EK-PPLV2-PM, page 5-24.
 */
static enum {
    s_NOTRASTER
    ,s_GETTINGPAN	/* Pixel Aspect ratio Numerator - vertical stretch */
    ,s_GETTINGPAD	/* Pixel Aspect ratio Denominator - horizontal */
    ,s_GETTINGHORIZ	/* Geometry horizontal extent (width) */
    ,s_GETTINGVERT	/* Geometry vertical extent (height) */
    ,s_RASTERDONE
} s_raster_state;
static int s_raster_params[s_RASTERDONE + 1];

/* SIXEL SCROLLING, which is on by default in VT3xx terminals, can be
 * turned off to better emulate VT2xx terminals by setting Sixel
 * Display Mode (DECSDM)
 *
 *		SIXEL DISPLAY MODE	SIXEL SCROLLING
 * VT125	Always on		Unsupported
 * VT240	Always on		Unsupported
 * VT241	Always on		Unsupported
 * VT330	Available via DECSDM	Default mode
 * VT382	Available via DECSDM	Default mode
 * VT340	Available via DECSDM	Default mode
 * VK100/GIGI	No sixel support	No sixel support
 *
 * dxterm (DECterm) emulated a VT100 series terminal, and supported sixels
 * according to 1995 posting to comp.os.vms:
 *	https://groups.google.com/g/comp.os.vms/c/XAUMmLtC8Yk
 * though not DRCS according to
 *	http://odl.sysworks.biz/disk$axpdocdec023/office/dwmot126/vmsdw126/relnotes/6470pro_004.html
 */

static void
init_sixel_background(Graphic *graphic, SixelContext const *context)
{
    int height = context->declared_height;
    int width = context->declared_width;
    TScreen *screen = TScreenOf(graphic->xw);

    if (height == 0) {		/* Paint bg from cursor to bottom row */
	height = (screen->bot_marg - graphic->charrow + 1) * FontHeight(screen);
	height = Min( height, graphic->max_height );
    }
    if (width == 0) {		/* Paint bg from cursor to right margin */
	width = (screen->rgt_marg - graphic->charcol + 1) * FontWidth(screen);
	width = Min( width, graphic->max_width );
    }

    TRACE(("initializing sixel background at (r%d,c%d) to size=%dx%d bgcolor=%hu\n",
	   graphic->charrow, graphic->charcol,
	   width, height,
	   context->background));

    if (context->background == COLOR_HOLE)
	return;

    if ( context->declared_width <= 0 && context->declared_height <= 0 )
    {
	/* No raster attributes, so make entire background opaque */
	init_graphic_background(graphic, context->background); 
    }
    else
    {
	graphic->displayed_width  = Max( graphic->displayed_width,
					 context->declared_width);
	graphic->displayed_height = Max( graphic->displayed_height,
					 context->declared_height);

	/* Make the selected rectangle opaque */
	graphic_memcpy_rectangle(graphic, 0, 0, width, height, context->background);
    }

    if ( !context->declared_height && height > graphic->displayed_height)
    {
	graphic->displayed_height = height;
    }
    if ( !context->declared_width &&  width > graphic->displayed_width)
    {
	graphic->displayed_width = width;
    }

    graphic->color_registers_used[context->background] = True;
}

static Boolean
set_sixel(Graphic *graphic, SixelContext const *context, int sixel)
{
    const RegisterNum color = context->current_register;

    TRACE2(("drawing sixel at pos=%d,%d color=%hu (hole=%d, [%d,%d,%d])\n",
	    context->col,
	    context->row,
	    color,
	    color == COLOR_HOLE,
	    ((color != COLOR_HOLE)
	     ? (unsigned) graphic->color_registers[color].r : 0U),
	    ((color != COLOR_HOLE)
	     ? (unsigned) graphic->color_registers[color].g : 0U),
	    ((color != COLOR_HOLE)
	     ? (unsigned) graphic->color_registers[color].b : 0U)));

    int width = (context->col + 1) * context->aspect_horizontal;
    if (context->col < 0 || width >= graphic->max_width)
    {
	TRACE(("sixel warning: column %d (aspect %d:%d): width %d exceeds max_width %d\n",
	       context->col, context->aspect_vertical, context->aspect_horizontal,
	       width, graphic->max_width));
	return False;
    }
    graphic->displayed_width  = Max(graphic->displayed_width,  width);

    int maxpix=-1;
    const int mh = graphic->max_height;
    const int mw = graphic->max_width;
    for (int pix = 0; pix < 6; pix++) {
	int pix_row = context->row + pix;
	int pix_col = context->col + (pix_row * mw);
	if (pix_row >= 0 &&
	    pix_row < mh) {
	    if (sixel & (1 << pix)) {
		maxpix = pix;
		SetSpixel(graphic, pix_col, color);
	    }
	} else {
	    TRACE(("sixel pixel %d out of bounds\n", pix));
	    return False;
	}
    }
    if (maxpix >= 0) {
	int height = (context->row + maxpix + 1) * context->aspect_vertical;
	graphic->displayed_height = Max(graphic->displayed_height, height);
    }
    return True;
}

static void
update_sixel_aspect(SixelContext * context, Graphic *graphic)
{
    int limit;
    int best;
    int gcd;

    /* We want to keep the ratio accurate but would like every pixel to have
     * the same size so keep these as whole numbers.
     */

    /* FIXME: The VT340 repeats pixels instead of spreading them out. */

    /* FIXME: A single sixel DCS string can have multiple aspect ratios on
       the VT340, but PPLv2 does not allow it. We currently neither implement
       it nor explicitly ignore DECGRA after sixels have started. We probably
       should allow for it as that is how genuine DEC hardware behaved. */

    TRACE(("sixel updating pixel aspect (v:h): %d:%d\n",
	   context->aspect_vertical, context->aspect_horizontal));

    /*
     * Reduce to the lowest possible format,
     * to handle ratios such as 2:3 and 16:9
     */
    limit = Min(context->aspect_vertical, context->aspect_horizontal);
    TRACE(("sixel aspect limit: %d\n", limit));
    best = 1;
    for (gcd = 2; gcd <= limit; ++gcd) {
	if ((context->aspect_vertical % gcd) == 0
	    && (context->aspect_horizontal % gcd) == 0) {
	    best = gcd;
	}
    }
    TRACE(("sixel aspect gcd: %d\n", best));
    context->aspect_vertical /= best;
    context->aspect_horizontal /= best;

    /* EK-PPLV2-PM-B01 says the range along either axis is no more than 10,
     * which is good advice for programs creating images for generic "Level 2
     * Sixel" devices.
     *
     * FIXME: The converse is true when implementing a sixel rendering
     * engine. As a *minimum* requirement, xterm must allow either axis to
     * range from 1 to 10, but, just like the hardware it emulates, it may
     * exceed specifications.
     *
     * The VT340 appears to have no practical limit. Even ratios over 480:1
     * -- where each pixel would exceed the screen height -- are allowed.
     */
#if 0
    if (context->aspect_vertical > (10 * context->aspect_horizontal))
	context->aspect_vertical = (10 * context->aspect_horizontal);

    if (context->aspect_horizontal > (10 * context->aspect_vertical))
	context->aspect_horizontal = (10 * context->aspect_vertical);

    /* in any case, limit "pixel" size if both are large */
#define by10(n) n = (n + 5) / 10
    while (context->aspect_vertical >= 10
	   && context->aspect_horizontal >= 10) {
	by10(context->aspect_vertical);
	by10(context->aspect_horizontal);
    }

    /* FIXME: Aspect Ratio is buggy, so we'll just force it off */
    graphic->pixw = 1;
    graphic->pixh = 1;

#else    

    /* VT340 approximates aspect ratio, so we will too, for now. */
    int min = (s_context.aspect_vertical < s_context.aspect_horizontal)?
	s_context.aspect_vertical:s_context.aspect_horizontal;
    s_context.aspect_vertical   = s_context.aspect_vertical / min;
    s_context.aspect_horizontal = s_context.aspect_horizontal / min;

    graphic->pixw = context->aspect_horizontal;
    graphic->pixh = context->aspect_vertical;
#endif

    TRACE(("sixel aspect ratio: %d:%d -> pixh=%d pixw=%d\n",
	   context->aspect_vertical,
	   context->aspect_horizontal,
	   graphic->pixh, graphic->pixw));
}

static int
finished_parsing(XtermWidget xw, Graphic *graphic)
{
    TScreen *screen = TScreenOf(xw);

    /* Update the screen scrolling and do a refresh.
     * The refresh may not cover the whole graphic.
     */
    if (screen->scroll_amt)
	FlushScroll(xw);

    if (SixelScrolling(xw)) {
	/* Note: XTerm follows the VT340 behavior in text cursor placement
	 * for nearly all sixel images.
	 *
	 * It differs in the few places where TWO newlines would have been
	 * necessary for subsequent text to not overwrite an image. XTerm
	 * always requires only a single newline. Emulating the VT340's
	 * quirky and undocumented behavior is of dubious value. The
	 * heuristic DEC used has nice properties, but only applies if the
	 * font is exactly 20 pixels high.
	 */
	int new_row, new_col;
	double row_delta = 0.0, col_delta = 0.0;

	new_row = graphic->charrow;
	if (graphic->displayed_height > 0) {
	    row_delta = graphic->displayed_height;
	    row_delta -= 1;	/* no increment when gfxheight == fontheight */ 
	    row_delta /= FontHeight(screen);
	    new_row += row_delta;
	}

	/* Normally, text cursor column does not change. */
	new_col = graphic->charcol;

	/* No DEC terminals did this, but it is a useful option */
	if (screen->sixel_scrolls_right) {
	    if (graphic->displayed_width > 0) {
		col_delta = graphic->displayed_width;
		col_delta -= 1;	/* no increment when gfxwidth == fontwidth */ 
		col_delta /= FontWidth(screen);
		new_col += col_delta;
	    }

	    if (new_col > screen->rgt_marg) {
		TRACE(("sixel: scrolling right exceeds right margin\n"));
#if 0
/* FIXME: Wrapping to the next line seems mistaken. sixel_scrolls_right is
 * for positioning text (or more graphics) to the right of the graphic we
 * just displayed. Wrapping could cause the screen to scroll up.
 * Probably, we should just delete this. */ 
		new_col = screen->lft_marg;
		new_row++;
#else
		/* Clamp to the margin. The user app can check if the cursor
		 * is in the last column and send a newline if they wish. */
		new_col = screen->rgt_marg;
#endif
		TRACE(("\t overriding to row=%d col=%d\n", new_row, new_col));
	    }
	}

	TRACE(("sixel: setting text position after %dx%d (pixw=%d, pixh=%d)\n"
	       "\t (row,col) delta: (%.1f, %.1f),\t old pos (%d %d),\t new: (%d,%d)\n",
	       graphic->displayed_width, graphic->displayed_height,
	       graphic->pixw, graphic->pixh,
	       row_delta, col_delta,
	       graphic->charrow, graphic->charcol,
	       new_row, new_col));

#if 0
	/* FIXME What is with the loop? Not needed for incremental drawing. */
	while (new_row > screen->bot_marg) {
	    xtermScroll(xw, 1);
	    new_row--;
	    TRACE(("bottom row was past screen.  new start row=%d, cursor row=%d\n",
		   graphic->charrow, new_row));
	}
#else
	int scl_amt = new_row - screen->bot_marg;
	if (scl_amt > 0) {
	    TRACE(("bottom row was past screen.  new start row=%d, cursor row=%d\n",
		   graphic->charrow, new_row));
	    xtermScroll(xw, scl_amt);
	    new_row = screen->bot_marg;
	}
#endif

	if (new_row < 0) {
	    TRACE(("WARNING: new cursor row was going to be negative (%d)!"
		   "This should never happen. Please send a bugreport.",
		   new_row));
	    new_row = 0;
	}
	set_cur_row(screen, new_row);
	set_cur_col(screen, new_col <= screen->rgt_marg ? new_col : screen->rgt_marg);
    }

    graphic->dirty = True;
    refresh_modified_displayed_graphics(xw);
    dump_graphic(graphic);
    return 0;
}

/*
 * Handle Sixel protocol selector: Ps1 ; Ps2 ; Ps3 q
 * Refer to EK-PPLV2-PM, Table 5-1 "Macro Parameter Selections"
 */
int
parse_sixel_init(XtermWidget xw, ANSI *params)
{
    s_xw = xw;
    s_screen = TScreenOf(xw);
    s_repeating = False;
    s_accumulator = -1;		/* No digits accumulated */
    s_context.aspect_vertical = 2;
    s_context.aspect_horizontal = 1;

    switch (s_screen->terminal_id) {
    case 240:
    case 241:
    case 330:
    case 340:
	s_context.aspect_vertical = 2;
	s_context.aspect_horizontal = 1;
	break;
    case 382:
	s_context.aspect_vertical = 1;
	s_context.aspect_horizontal = 1;
	break;
    default:
	s_context.aspect_vertical = 2;
	s_context.aspect_horizontal = 1;
	break;
    }

    s_context.declared_width = s_context.declared_height = 0;
    s_context.row = s_context.col = 0;
    s_prev_row = s_prev_col = 0;
    s_color_state = s_raster_state = 0;
    
    /* default isn't white on the VT240, but not sure what it is */
    s_context.current_register = 3;	/* FIXME: using green, but not sure what it should be */

    /* allocate s_graphic if needs be  */
    if (SixelScrolling(s_xw)) {
	TRACE(("sixel scrolling enabled: inline positioning for graphic at %d,%d\n",
	       s_screen->cur_row, s_screen->cur_col));
	s_graphic = get_new_graphic(s_xw,
				    s_screen->cur_row,
				    s_screen->cur_col, 0U);
    } else {
	TRACE(("sixel scrolling disabled: inline positioning for graphic at %d,%d\n",
	       0, 0));
	s_graphic = get_new_graphic(s_xw, 0, 0, 0U);
    }

    {
	static const int vertical[] =
	{
	/*    0    1    2    3    4    5    6    7    8    9 */
	    200, 200, 450, 300, 250, 183, 150, 130, 112, 100
	};
	int Pmacro = UParmOf(params->a_param[0]);
	int Pbgmode = UParmOf(params->a_param[1]);
	int Phgrid = UParmOf(params->a_param[2]);

	TRACE(("sixel bitmap graphics sequence: params=%d (Pmacro=%d Pbgmode=%d Phgrid=%d) scroll_amt=%d\n",
	       params->a_nparam,
	       Pmacro,
	       Pbgmode,
	       Phgrid,
	       s_screen->scroll_amt));

	/* Ps1: "Macro"  Aspect ratio and default grid size */
	/*
	 * Note: Macro is often left at 0 since its horizontal grid size can be
	 * overridden by Ps3 and its aspect ratio can be overridden by DECGRA.
	 */
	if (Pmacro > 9 || Pmacro < 0) {
	    Pmacro = 0;
	}
	s_context.aspect_vertical = vertical[Pmacro];
	s_context.aspect_horizontal = 100;

	/* Ps2: Background  0 or 2 = opaque, 1 = transparent */
	if (Pbgmode == 1) {
	    s_context.background = COLOR_HOLE;
	} else {
	    /* The default background register is always zero (including in
	     * light background mode) on the VT340.
	     */
	    s_context.background = 0;
	}

	/* Ps3: horizontal grid size in decipoints (1/720 inch) */
	/*
	 * Note: The CRT of the VT340 had a grid (distance between pixels)
	 * of 10/720th of an inch (72dpi == 0.0195 cm).
	 */
	if (Phgrid <= 0) {
	    Phgrid = 50;	/* Default is 50 decipoints (144dpi) */
	}
    }

    update_sixel_aspect(&s_context, s_graphic);

    return 0;
}

#ifdef _POSIX_MONOTONIC_CLOCK
#define TS_NANOSEC     1L
#define TS_MICROSEC 1000L * TS_NANOSEC
#define TS_MILLISEC 1000L * TS_MICROSEC
#define TS_SEC      1000L * TS_MILLISEC

/* Returns True if the system's monotonic clock has reached or exceeded _when_.
 * If _increment_ is not NULL, _when_ will be set to now + _increment_.
 */
static Boolean
times_up(struct timespec *when, struct timespec *increment)
{
    struct timespec now;

    clock_gettime(CLOCK_MONOTONIC, &now);
    if (when->tv_sec < now.tv_sec
	|| (when->tv_sec == now.tv_sec
	    && when->tv_nsec < now.tv_nsec)) {
	return False;
    }
    if (increment) {
	when->tv_sec += now.tv_sec + increment->tv_sec;
	when->tv_nsec += now.tv_nsec + increment->tv_nsec;
	while (when->tv_nsec >= TS_SEC) {
	    when->tv_sec += 1;
	    when->tv_nsec -= TS_SEC;
	}
    }
    return True;
}

#endif

static void
parse_sixel_incremental_display(void)
{
    /* Watch sixels appear just like a VT340!  */
    int dirty_row = ((s_context.row * s_context.aspect_vertical)
		     + (s_graphic->charrow * FontHeight(s_screen)));

    int dirty_col = ((s_context.col * s_context.aspect_horizontal)
		     + (s_graphic->charcol * FontWidth(s_screen)));

#ifdef _POSIX_MONOTONIC_CLOCK
    static struct timespec next_refresh =
    {
	0, 0
    };
    static struct timespec refresh_delay =
    {
	0, TS_MILLISEC
    };

    /* Bundle up incremental refreshes that happen faster than visually
     * perceptible.
     */
    if (!times_up(&next_refresh, &refresh_delay)) {
	return;
    }
#endif

    if (dirty_row != s_prev_row) {
	s_prev_row = dirty_row;
	s_prev_col = 0;
    }

    /* FIXME: Image should be cropped to the text cells when xtermmargin>0 */
    s_graphic->dirty = True;
    refresh_displayed_graphics(s_xw,
			       s_prev_col / FontWidth(s_screen),
			       s_prev_row / FontHeight(s_screen),
			       1 + 1 + (dirty_col - s_prev_col) /
			       FontWidth(s_screen),
			       1);
    s_prev_row = dirty_row;
    s_prev_col = dirty_col;
}

/* Helper for parse_sixel_char()
 * Move graphic cursor down and maybe scroll the terminal.
 * FIXME: merge incremental & non-incremental scrolling.
 */
static void
gnl_scroll(void)
{
   /* FIXME: this algorithm is not correct. */ 
    int whatisthis =   s_graphic->displayed_height
		     - s_context.row * s_context.aspect_vertical;
    int scroll_lines = 
	  (
	      s_context.row * s_context.aspect_vertical
	    + Min(s_context.aspect_vertical * 6, whatisthis) 
	    - 1
	  )
	/ FontHeight(s_screen)
	+ s_graphic->charrow
	- s_screen->bot_marg
	;
    if (scroll_lines < 0)
	scroll_lines = 0;

    /* If we hit the bottom margin on the graphics page (well, we just use
     * the text margin for now), the behavior is to either scroll or to
     * discard the remainder of the graphic depending on SixelScrolling.
     */
    if (scroll_lines > 0) {
	if (SixelScrolling(s_xw)) {
	    xtermScroll(s_xw, scroll_lines);
	    if (s_screen->incremental_graphics) {
		FlushScroll(s_xw);
		XSync(s_screen->display, False);
		s_graphic->dirty = True;
		refresh_modified_displayed_graphics(s_xw);
	    }
	    TRACE(("graphic scrolled the screen %d lines. s_screen->scroll_amt=%d s_screen->topline=%d, now starting row is %d\n",
		   scroll_lines,
		   s_screen->scroll_amt,
		   s_screen->topline,
		   s_graphic->charrow));
	}
    }
}
    
int
parse_sixel_char(char cp)
{
    /* s_* variables are static state, defined above */

    if (cp == '\0' || isspace(cp)) {
	if (s_repeating && cp == '\0') {
	    TRACE(("DATA_ERROR: sixel data string terminated in the middle of a repeat operator\n"));
	    s_repeating = False;
	    return finished_parsing(s_xw, s_graphic);
	}
	return 0;
    }

    if (isdigit(cp)) {
	if (s_accumulator == -1)
	    s_accumulator = 0;
	s_accumulator *= 10;
	s_accumulator += cp - '0';
	return 0;
    }

    if (s_repeating) {		/* '!' ...  */
	/* Not space or digit, so it must be the sixel to show */
	if (cp >= 0x3F && cp <= 0x7E) {
	    int sixel = cp - 0x3f;
	    TRACE(("sixel repeat operator: count=%d, "
		   "sixel=" SIXEL_PRINTF  " ('%c')\n",
		   s_accumulator, sixel, (char) cp));
	    if (s_accumulator <= 0) {
		/* If the repeat count is zero or omitted, it is treated as 1 */
		s_accumulator = 1;
	    }
	    if (!s_graphic->valid) {
		init_sixel_background(s_graphic, &s_context);
		s_graphic->valid = True;
	    }
	    if (sixel) {
		for (int i = 0; i < s_accumulator; i++) {
		    if (!set_sixel(s_graphic, &s_context, sixel)) {
			break;
		    }
		    s_context.col++;
		}
	    } else {
		s_context.col += s_accumulator;
	    }
	} else {
	    TRACE(("DATA_ERROR: sixel data string ignoring repeat operator followed by control: %c (%d)\n",
		   cp, cp));
	    s_repeating = False;
	}
	s_accumulator = -1;
	s_repeating = False;
	if (s_screen->incremental_graphics)
	    parse_sixel_incremental_display();
	return 0;
    }

    /* FIXME: Raster attributes (") can occur repeatedly and at any time. */
    /* Parse the Raster Attributes ( " Pn1 ; Pn2 ; Pn3 ; Pn4 )  */
    if (s_raster_state > s_NOTRASTER) {
	TRACE(("sixel DECGRA raster argument found %d, followed by '%c'\n",
	       s_accumulator, cp));

	/* cp is not a digit or space, so save the accumulator */
	if (s_accumulator != -1) {
	    s_raster_params[s_raster_state] = s_accumulator;
	}

	if (s_raster_params[s_raster_state] <= 0) {	/* -1 means omitted */
	    s_raster_params[s_raster_state] = 1;	/* Default */
	}

	switch (s_raster_state) {
	case s_GETTINGPAN:	/* Pn1: Pixel Aspect Numerator: 1 to MAXINT */
	    s_context.aspect_vertical   = s_raster_params[s_GETTINGPAN];
	    update_sixel_aspect(&s_context, s_graphic);
	    break;
	case s_GETTINGPAD:	/* Pn2: Pixel Aspect Denominator: 1 to MAXINT */
	    s_context.aspect_horizontal = s_raster_params[s_GETTINGPAD];
	    update_sixel_aspect(&s_context, s_graphic);
	    break;
	case s_GETTINGHORIZ:	/* Pn3: Horizontal extent: 1 to MAXINT */
	    if (s_raster_params[s_GETTINGHORIZ] > s_graphic->max_width) {
		TRACE(("DATA_WARNING: "
		       "truncating raster width %d to max_width of %d\n",
		       s_raster_params[s_GETTINGHORIZ], s_graphic->max_width));
		s_raster_params[s_GETTINGHORIZ] = s_graphic->max_width;
	    }
	    if (s_raster_params[s_GETTINGHORIZ] >= 0)
		s_context.declared_width = s_raster_params[s_GETTINGHORIZ];
	    break;
	case s_GETTINGVERT:	/* Pn4: Vertical extent: 1 to MAXINT */
	    if (s_raster_params[s_GETTINGVERT] > s_graphic->max_height) {
		TRACE(("DATA_WARNING: "
		       "truncating raster height %d to max_height of %d\n",
		       s_raster_params[s_GETTINGVERT], s_graphic->max_height));
		s_raster_params[s_GETTINGVERT] = s_graphic->max_height;
	    }
	    s_context.declared_height = s_raster_params[s_GETTINGVERT];
	    break;

#define NEVERHAPPENS(type, state)		\
	case state: \
	    TRACE(("DATA_ERROR: impossible state " #state " (%d) occurred.\n" \
		   "\t next char %c (%d)\n", s_ ## type ## _state, cp, cp)); \
	    s_ ## type ## _state = 0; \
	    return finished_parsing(s_xw, s_graphic); \
	    break

	NEVERHAPPENS(raster, s_RASTERDONE);
	NEVERHAPPENS(raster, s_NOTRASTER);

	default:
	    TRACE(("DATA_ERROR: raster operator ('\"') with too many parameters (%d)\n, next char %c (%d)\n",
		   s_raster_state, cp, cp));
	    s_raster_state = s_NOTRASTER;
	    return finished_parsing(s_xw, s_graphic);
	}

	s_accumulator = -1;
	s_raster_state++;

	if (cp == ';') {
	    return 0;
	}

	/* cp (next character to consume) is not digit, space, or semicolon, so finish up with raster  */
	s_raster_state = s_NOTRASTER;

	/* FALLTHRU TO PROCESS cp */
    }

    if (s_color_state > s_NOTCOLORING) {
	/* cp is not a digit or space, so process the accumulator */
	if (s_accumulator != -1) {
	    s_color_params[s_color_state] = s_accumulator;
	}
	switch (s_color_state) {
	case s_GETTINGREGISTER:
	    if (s_accumulator == -1) {
		/* FIXME: What does VT340 do with default register? */
		TRACE(("DATA_ERROR: sixel data string uses default color register, next char %c (%d)\n",
		       cp, cp));
		return finished_parsing(s_xw, s_graphic);
	    }
	    s_Pregister = (RegisterNum) s_color_params[s_GETTINGREGISTER];
	    /* The DEC terminals wrapped register indices. */
	    s_Pregister %= (RegisterNum) s_graphic->valid_registers;
	    TRACE(("sixel switch to color register=%u\n", s_Pregister));
	    s_context.current_register = s_Pregister;
	    break;
	case s_GETTINGCOLORSPACE:
	    if (s_accumulator == -1) {
		/* FIXME: Default VT340 colorspace is HSL, right? */
		TRACE(("DATA_ERROR: sixel data string uses default colorspace \n"));
		s_color_state = s_NOTCOLORING;
		return finished_parsing(s_xw, s_graphic);
	    }
	    break;
	case s_GETTINGPC1:
	    if (s_accumulator == -1) {
		/* FIXME: Does VT340 sixel do the same as ReGIS and use the previous value for unspecified color components?   */
		TRACE(("DATA_ERROR: sixel data string uses default color component 1 \n"));
		s_color_state = s_NOTCOLORING;
		return finished_parsing(s_xw, s_graphic);
	    }
	    break;
	case s_GETTINGPC2:
	    if (s_accumulator == -1) {
		/* FIXME: unspecified color components?   */
		TRACE(("DATA_ERROR: sixel data string uses default color component 2 \n"));
		s_color_state = s_NOTCOLORING;
		return finished_parsing(s_xw, s_graphic);
	    }
	    break;
	case s_GETTINGPC3:
	    if (s_accumulator == -1) {
		/* FIXME: unspecified color components?   */
		TRACE(("DATA_ERROR: sixel data string uses default color component 3 \n"));
		s_color_state = s_NOTCOLORING;
		return finished_parsing(s_xw, s_graphic);
	    }
	    break;
	case s_COLORINGDONE:
	case s_NOTCOLORING:
	    /* ignore unreachable states */
	    break;
	default:
	    /* FIXME: Why abort if too many parameters? Doesn't DEC ignore unknown parameters?  */
	    TRACE(("DATA_ERROR: sixel switch color operator ('#') with too many parameters\n, next char %c (%d)\n",
		   cp, cp));
	    s_color_state = s_NOTCOLORING;
	    return finished_parsing(s_xw, s_graphic);
	}

	s_accumulator = -1;
	s_color_state++;

	if (cp == ';') {
	    return 0;
	} else {
	    /* cp (next character to consume) is not digit, space, or semicolon, so finish up with color  */
	    if (s_color_state != s_COLORINGDONE && s_color_state != s_GETTINGCOLORSPACE) {
		TRACE(("DATA_ERROR: sixel switch color operator with wrong number of parameters (%d)\n", s_color_state));
		s_color_state = s_NOTCOLORING;
		return finished_parsing(s_xw, s_graphic);
	    }

	    if (s_color_state == s_COLORINGDONE) {
		/* We've got all components, so set the color register */
		int Pspace = s_color_params[s_GETTINGCOLORSPACE];
		int Pc1 = s_color_params[s_GETTINGPC1];
		int Pc2 = s_color_params[s_GETTINGPC2];
		int Pc3 = s_color_params[s_GETTINGPC3];
		short r, g, b;

		s_color_state = s_NOTCOLORING;
		TRACE(("sixel set color register=%u space=%d color=[%d,%d,%d]\n",
		       s_Pregister, Pspace, Pc1, Pc2, Pc3));

		switch (Pspace) {
		case 1:	/* HLS */
		    if (Pc1 > 360 || Pc2 > 100 || Pc3 > 100) {
			TRACE(("DATA_ERROR: sixel set color operator uses out-of-range HLS color coordinates %d,%d,%d\n",
			       Pc1, Pc2, Pc3));
			s_color_state = s_NOTCOLORING;
			return finished_parsing(s_xw, s_graphic);
		    }
		    hls2rgb(Pc1, Pc2, Pc3, &r, &g, &b);
		    break;
		case 2:	/* RGB */
		    if (Pc1 > 100 || Pc2 > 100 || Pc3 > 100) {
			TRACE(("DATA_ERROR: sixel set color operator uses out-of-range RGB color coordinates %d,%d,%d\n",
			       Pc1, Pc2, Pc3));
			s_color_state = s_NOTCOLORING;
			return finished_parsing(s_xw, s_graphic);
		    }
		    r = (short) Pc1;
		    g = (short) Pc2;
		    b = (short) Pc3;
		    break;
		default:	/* unknown */
		    TRACE(("DATA_ERROR: sixel set color operator uses unknown color space %d\n", Pspace));
		    s_color_state = s_NOTCOLORING;
		    return finished_parsing(s_xw, s_graphic);
		}
		update_color_register(s_graphic,
				      s_Pregister,
				      r, g, b);
	    }

	    s_color_state = s_NOTCOLORING;
	    /* FALLTHRU TO PROCESS cp */
	}
    }

    if (cp >= 0x3f && cp <= 0x7e) {
	int sixel = cp - 0x3f;
	TRACE(("sixel=" SIXEL_PRINTF " ('%c')\n", sixel, (char) cp));
	if (!s_graphic->valid) {
	    init_sixel_background(s_graphic, &s_context);
	    s_graphic->valid = True;
	}
	if (sixel) {
	    if (!set_sixel(s_graphic, &s_context, sixel)) {
		return 0;
	    }
	}
	s_context.col++;
	if (s_screen->incremental_graphics)
	    parse_sixel_incremental_display();
    } else if (cp == '$') {	/* DECGCR */
	/* ignore DECCRNLM in sixel mode */
	TRACE(("sixel: DECGCR Graphic Carriage Return\n"));
	s_context.col = 0;
    } else if (cp == '-') {	/* DECGNL */
	TRACE(("sixel: DECGNL Graphic New Line\n"));
	s_context.col = 0;
	s_context.row += 6;
	s_graphic->displayed_height = Max(s_graphic->displayed_height, s_context.row);
	gnl_scroll();
	TRACE2(("sixel: new graphic row location is %u\n", s_context.row));
    } else if (cp == '!') {	/* DECGRI */
	s_repeating = True;
	s_accumulator = -1;
    } else if (cp == '#') {	/* DECGCI */
	s_color_state = s_GETTINGREGISTER;
	s_accumulator = -1;
    } else if (cp == '"') {	/* DECGRA */
	s_raster_state = s_GETTINGPAN;
	s_accumulator = -1;
	s_raster_params[s_GETTINGPAN] = 1;	/* Default if not specified */
	s_raster_params[s_GETTINGPAD] = 1;	/* Default if not specified */
	s_raster_params[s_GETTINGHORIZ] = 0;	/* Default if not specified */
	s_raster_params[s_GETTINGVERT] = 0;	/* Default if not specified */
    } else {
	TRACE(("DATA_ERROR: skipping unknown sixel command %04x (%c)\n",
	       (int) cp, cp));
    }

    return 0;
}


/* Just like finished_parsing, but called from do_dcs in misc.c */
int
parse_sixel_finished(XtermWidget xw)
{
    /* FIXME: should use s_xw */
    return finished_parsing(xw, s_graphic);
}
