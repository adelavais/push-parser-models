/* A Bison parser, made by GNU Bison 3.7.4.143-8ab6-dirty.  */

/* Skeleton implementation for Bison LALR(1) parsers in D

   Copyright (C) 2007-2012, 2019-2020 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */


version(D_Version2) {
} else {
  static assert(false, "need compiler for D Version 2");
}




import std.format;
import std.conv;

/**
 * Handle error message internationalisation.
 */
static if (!is(typeof(YY_))) {
  version(YYENABLE_NLS)
  {
    version(ENABLE_NLS)
    {
      extern(C) char* dgettext(const char*, const char*);
      string YY_(const char* s)
      {
        return to!string(dgettext("bison-runtime", s));
      }
    }
  }
  static if (!is(typeof(YY_)))
  {
    pragma(inline, true)
    string YY_(string msg) { return msg; }
  }
}

/**
 * A Bison parser, automatically generated from <tt>calc.y</tt>.
 *
 * @author LALR (1) parser skeleton written by Paolo Bonzini.
 * Port to D language was done by Oliver Mangold.
 */

/**
 * Communication interface between the scanner and the Bison-generated
 * parser <tt>Calc</tt>.
 */
public interface Lexer
{
  /**
   * Entry point for the scanner.  Returns the token identifier corresponding
   * to the next token and prepares to return the semantic value
   * and beginning/ending positions of the token.
   * @return the token identifier corresponding to the next token. */
  Symbol yylex ();

  /**
   * Entry point for error reporting.  Emits an error
   * referring to the given location in a user-defined way.
   *
   * @param loc The location of the element to which the
   *                error message is related
   * @param s The string for the error message.  */
   void yyerror (const Location loc, string s);

}


alias Symbol = Calc.Symbol;
alias Value = YYSemanticType;
alias Location = YYLocation;
alias Position = YYPosition;



  /**
   * A struct denoting a point in the input.*/
public struct YYPosition {

  /** The column index within the line of input.  */
  public int column = 1;
  /** The line number within an input file.  */
  public int line = 1;
  /** The name of the input file.  */
  public string filename = null;

  /**
   * A string representation of the position. */
  public string toString() const {
    if (filename)
      return format("%s:%d.%d", filename, line, column);
    else
      return format("%d.%d", line, column);
  }
}

/**
 * A struct defining a pair of positions.  Positions, defined by the
 * <code>Position</code> struct, denote a point in the input.
 * Locations represent a part of the input through the beginning
 * and ending positions.  */
public struct YYLocation
{
  /** The first, inclusive, position in the range.  */
  public Position begin;

  /** The first position beyond the range.  */
  public Position end;

  /**
   * Create a <code>Location</code> denoting an empty range located at
   * a given point.
   * @param loc The position at which the range is anchored.  */
  public this (Position loc) {
    this.begin = this.end = loc;
  }

  /**
   * Create a <code>Location</code> from the endpoints of the range.
   * @param begin The first position included in the range.
   * @param end   The first position beyond the range.  */
  public this (Position begin, Position end)
  {
    this.begin = begin;
    this.end = end;
  }

  /**
   * A representation of the location.
   */
  public string toString () const {
    auto end_col = 0 < end.column ? end.column - 1 : 0;
    auto res = begin.toString ();
    if (end.filename && begin.filename != end.filename)
      res ~= "-" ~ format("%s:%d.%d", end.filename, end.line, end_col);
    else if (begin.line < end.line)
      res ~= "-" ~ format("%d.%d", end.line, end_col);
    else if (begin.column < end_col)
      res ~= "-" ~ format("%d", end_col);
    return res;
  }
}

private immutable bool yy_location_is_class = false;

private union YYSemanticType
{
#line 29 "calc.y"

  int ival;

#line 187 "calc.d"

};
/* Token kinds.  */
public enum TokenKind {
  YYEMPTY = -2,
  YYEOF = 0,
  YYerror = 1,
  YYUNDEF = 2,
  PLUS = 3,
  MINUS = 4,
  STAR = 5,
  SLASH = 6,
  LPAR = 7,
  RPAR = 8,
  EOL = 9,
  NUM = 10,
  UNARY = 11,
}

class Calc
{
  /** Version number for the Bison executable that generated this parser.  */
  public static immutable string yy_bison_version = "3.7.4.143-8ab6-dirty";

  /** Name of the skeleton that generated this parser.  */
  public static immutable string yy_bison_skeleton = "lalr1.d";


  /* Symbol kinds.  */
  struct SymbolKind
  {
    enum
    {
    YYEMPTY = -2,  /* No symbol.  */
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 1,                   /* error  */
    YYUNDEF = 2,                   /* "invalid token"  */
    PLUS = 3,                      /* "+"  */
    MINUS = 4,                     /* "-"  */
    STAR = 5,                      /* "*"  */
    SLASH = 6,                     /* "/"  */
    LPAR = 7,                      /* "("  */
    RPAR = 8,                      /* ")"  */
    EOL = 9,                       /* "end of line"  */
    NUM = 10,                      /* "number"  */
    UNARY = 11,                    /* UNARY  */
    YYACCEPT = 12,                 /* $accept  */
    input = 13,                    /* input  */
    line = 14,                     /* line  */
    exp = 15,                      /* exp  */
    }

    private int yycode_;
    alias yycode_ this;

    this(int code)
    {
      yycode_ = code;
    }

    /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
    final void toString(W)(W sink) const
    if (isOutputRange!(W, char))
    {
      immutable string[] yy_sname = [
  "end of file", "error", "invalid token", "+", "-", "*", "/", "(", ")",
  "end of line", "number", "UNARY", "$accept", "input", "line", "exp", null
      ];

      put(sink, yy_sname[yycode_]);
    }
  }



  private final Location yylloc_from_stack (ref YYStack rhs, int n)
  {
    static if (yy_location_is_class) {
      if (n > 0)
        return new Location (rhs.locationAt (n-1).begin, rhs.locationAt (0).end);
      else
        return new Location (rhs.locationAt (0).end);
    } else {
      if (n > 0)
        return Location (rhs.locationAt (n-1).begin, rhs.locationAt (0).end);
      else
        return Location (rhs.locationAt (0).end);
    }
  }


  /** The object doing lexical analysis for us.  */
  private Lexer yylexer;





  /**
   * Instantiate the Bison-generated parser.
   * @param yylexer The scanner that will supply tokens to the parser.
   */
  public this (Lexer yylexer) {
    this.yylexer = yylexer;
    this.yyDebugStream = stderr;

  }

  private File yyDebugStream;

  /**
   * The <tt>File</tt> on which the debugging output is
   * printed.
   */
  public File getDebugStream () { return yyDebugStream; }

  /**
   * Set the <tt>std.File</tt> on which the debug output is printed.
   * @param s The stream that is used for debugging output.
   */
  public final void setDebugStream(File s) { yyDebugStream = s; }

  private int yydebug = 0;

  /**
   * Answer the verbosity of the debugging output; 0 means that all kinds of
   * output from the parser are suppressed.
   */
  public final int getDebugLevel() { return yydebug; }

  /**
   * Set the verbosity of the debugging output; 0 means that all kinds of
   * output from the parser are suppressed.
   * @param level The verbosity level for debugging output.
   */
  public final void setDebugLevel(int level) { yydebug = level; }

  protected final void yycdebug (string s) {
    if (0 < yydebug)
      yyDebugStream.write (s);
  }

  protected final void yycdebugln (string s) {
    if (0 < yydebug)
      yyDebugStream.writeln (s);
  }

  private final Calc.Symbol yylex () {
    return yylexer.yylex ();
  }

  protected final void yyerror (const Location loc, string s) {
    yylexer.yyerror (loc, s);
  }

  /**
   * The number of syntax errors so far.
   */
  public int numberOfErrors() const { return yynerrs_; }
  private int yynerrs_ = 0;

  /**
   * Returned by a Bison action in order to stop the parsing process and
   * return success (<tt>true</tt>).  */
  public static immutable int YYACCEPT = 0;

  /**
   * Returned by a Bison action in order to stop the parsing process and
   * return failure (<tt>false</tt>).  */
  public static immutable int YYABORT = 1;

  /**
   * Returned by a Bison action in order to request a new token.
   */
  public static immutable int YYPUSH_MORE = 4;

  /**
   * Returned by a Bison action in order to start error recovery without
   * printing an error message.  */
  public static immutable int YYERROR = 2;

  // Internal return codes that are not supported for user semantic
  // actions.
  private static immutable int YYERRLAB = 3;
  private static immutable int YYNEWSTATE = 4;
  private static immutable int YYDEFAULT = 5;
  private static immutable int YYREDUCE = 6;
  private static immutable int YYERRLAB1 = 7;
  private static immutable int YYRETURN = 8;
  private static immutable int YYGETTOKEN = 9; /* Signify that a new token is expected when doing push-parsing.  */


  private static immutable YYSemanticType yy_semantic_null;
  private int yyerrstatus_ = 0;

  private void yyerrok()
  {
    yyerrstatus_ = 0;
  }

  /**
   * Whether error recovery is being done.  In this state, the parser
   * reads token until it reaches a known state, and then restarts normal
   * operation.  */
  public final bool recovering ()
  {
    return yyerrstatus_ == 0;
  }

  /** Compute post-reduction state.
   * @param yystate   the current state
   * @param yysym     the nonterminal to push on the stack
   */
  private int yyLRGotoState(int yystate, int yysym) {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  private int yyaction (int yyn, ref YYStack yystack, int yylen)
  {
    Value yyval;
    Location yyloc = yylloc_from_stack (yystack, yylen);

    /* If YYLEN is nonzero, implement the default value of the action:
       `$$ = $1'.  Otherwise, use the top of the stack.

       Otherwise, the following line sets YYVAL to garbage.
       This behavior is undocumented and Bison
       users should not rely upon it.  */
    if (yylen > 0)
      yyval = yystack.valueAt (yylen - 1);
    else
      yyval = yystack.valueAt (0);


    yy_reduce_print (yyn, yystack);

    switch (yyn)
    {
    case 5: /* line: exp "end of line"  */
#line 58 "calc.y"
                    { writeln (((yystack.valueAt (1)).ival)); }
      break;

    case 6: /* line: error "end of line"  */
#line 59 "calc.y"
                    { yyerrok(); }
      break;

    case 7: /* exp: "number"  */
#line 63 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (0)).ival); }
      break;

    case 8: /* exp: exp "+" exp  */
#line 64 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) + ((yystack.valueAt (0)).ival); }
      break;

    case 9: /* exp: exp "-" exp  */
#line 65 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) - ((yystack.valueAt (0)).ival); }
      break;

    case 10: /* exp: exp "*" exp  */
#line 66 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) * ((yystack.valueAt (0)).ival); }
      break;

    case 11: /* exp: exp "/" exp  */
#line 67 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) / ((yystack.valueAt (0)).ival); }
      break;

    case 12: /* exp: "+" exp  */
#line 68 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (0)).ival); }
      break;

    case 13: /* exp: "-" exp  */
#line 69 "calc.y"
                       { (yyval.ival) = -((yystack.valueAt (0)).ival); }
      break;

    case 14: /* exp: "(" exp ")"  */
#line 70 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (1)).ival); }
      break;


#line 478 "calc.d"

      default: break;
    }


    yy_symbol_print ("-> $$ =", to!SymbolKind (yyr1_[yyn]), yyval, yyloc);

    yystack.pop (yylen);
    yylen = 0;

    /* Shift the result of the reduction.  */
    int yystate = yyLRGotoState(yystack.stateAt(0), yyr1_[yyn]);
    yystack.push (yystate, yyval, yyloc);
    return YYNEWSTATE;
  }


  /*--------------------------------.
  | Print this symbol on YYOUTPUT.  |
  `--------------------------------*/

  private final void yy_symbol_print (string s, SymbolKind yykind,
    ref Value yyval, ref Location yyloc)
  {
    if (0 < yydebug)
    {
      File yyo = yyDebugStream;
      yyo.write(s);
      yyo.write(yykind < yyntokens_ ? " token " : " nterm ");
      yyo.write(format("%s", yykind));
      yyo.write(" (" ~ yyloc.toString() ~ ": ");
      switch (yykind)
    {
    case SymbolKind.NUM: /* "number"  */
#line 43 "calc.y"
         { yyo.write((yyval.ival)); }
#line 515 "calc.d"
        break;

    case SymbolKind.exp: /* exp  */
#line 43 "calc.y"
         { yyo.write((yyval.ival)); }
#line 521 "calc.d"
        break;

      default:
        break;
    }
      yyo.write(")\n");
    }
  }


  /**
    * A complete symbol
    */
  struct Symbol
  {
    private SymbolKind kind;
    private Value value_;
    private Location location_;
    this(TokenKind token, Location loc)
    {
      kind = yytranslate_(token);
      location_ = loc;
    }
    static foreach (member; __traits(allMembers, YYSemanticType))
    {
      this(TokenKind token, typeof(mixin("YYSemanticType." ~ member)) val, Location loc)
      {
        kind = yytranslate_(token);
        mixin("value_." ~ member ~ " = val;");
        location_ = loc;
      }
    }
    SymbolKind token() { return kind; }
    Value value() { return value_; }
    Location location() { return location_; }
  }

  // Lookahead symbol kind.
    SymbolKind yytoken = SymbolKind.YYEMPTY;

    /* State.  */
    int yyn = 0;
    int yylen = 0;
    int yystate = 0;
    YYStack yystack;
    int label = YYNEWSTATE;

    /// The location where the error started.
    Location yyerrloc;

    /// Location of the lookahead.
    Location yylloc;

    /// @$.
    Location yyloc;

    /// Semantic value of the lookahead.
    Value yylval;

  /**
   * Parse input from the scanner that was specified at object construction
   * time.  Return whether the end of the input was reached successfully.
   *
   * @return <tt>true</tt> if the parsing succeeds.  Note that this does not
   *          imply that there were no syntax errors.
   */

  int parse()
  {
    int status = 0;
    do {
      status = this.push_parse(yylex());
    } while (status == YYPUSH_MORE);
    return status;
  }


  public int push_parse(Symbol yyla)
  {
    if (!this.push_parse_initialized)
    {
      push_parse_initialize();
      yyerrstatus_ = 0;
    }
    else
      label = YYGETTOKEN;

    bool push_token_consumed = true;

    // Discard the LAC context in case there still is one left from a
    // previous invocation.
    yylacDiscard("init");

    yycdebugln ("Starting parse");

    /* Initialize the stack.  */
    yystack.push (yystate, yylval, yylloc);

    for (;;)
      final switch (label)
      {
        /* New state.  Unlike in the C/C++ skeletons, the state is already
           pushed when we come here.  */
      case YYNEWSTATE:
        yycdebugln (format("Entering state %d", yystate));
        if (0 < yydebug)
          yystack.print (yyDebugStream);

        /* Accept?  */
        if (yystate == yyfinal_)// ]b4_push_if([[]], [[]])[
        {
          label = YYACCEPT;
          break;
        }

        /* Take a decision.  First try without lookahead.  */
        yyn = yypact_[yystate];
        if (yyPactValueIsDefault(yyn))
        {
          label = YYDEFAULT;
          break;
        }
        goto case;

        case YYGETTOKEN:

        /* Read a lookahead token.  */
        if (yytoken == SymbolKind.YYEMPTY)
        {
          if (!push_token_consumed)
            return YYPUSH_MORE;

          yycdebugln ("Reading a token");
          yytoken = yyla.token;
          yylval = yyla.value;
          yylloc = yyla.location;
          push_token_consumed = false;
        }

        /* Token already converted to internal form.  */
        yy_symbol_print ("Next token is", yytoken, yylval, yylloc);

        if (yytoken == SymbolKind.YYerror)
        {
          // The scanner already issued an error message, process directly
          // to error recovery.  But do not keep the error token as
          // lookahead, it is too special and may lead us to an endless
          // loop in error recovery. */
          yytoken = SymbolKind.YYUNDEF;
          yyerrloc = yylloc;
          label = YYERRLAB1;
        }
        else
        {
          /* If the proper action on seeing token YYTOKEN is to reduce or to
             detect an error, take that action.  */
          yyn += yytoken;
          if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yytoken) {
            if (!yylacEstablish(yystack, yytoken))
              label = YYERRLAB;
            else
              label = YYDEFAULT;
          }
          /* <= 0 means reduce or error.  */
          else if ((yyn = yytable_[yyn]) <= 0)
          {
            if (yyTableValueIsError(yyn))
              label = YYERRLAB;
            else if (!yylacEstablish(yystack, yytoken))
              label = YYERRLAB;
            else
            {
              yyn = -yyn;
              label = YYREDUCE;
            }
          }
          else
          {
            /* Shift the lookahead token.  */
            yy_symbol_print ("Shifting", yytoken, yylval, yylloc);

            /* Discard the token being shifted.  */
            yytoken = SymbolKind.YYEMPTY;

            /* Count tokens shifted since error; after three, turn off error
             * status.  */
            if (yyerrstatus_ > 0)
              --yyerrstatus_;

            yystate = yyn;
            yystack.push (yystate, yylval, yylloc);
            yylacDiscard("shift");
            label = YYNEWSTATE;
          }
        }
        break;

      /*-----------------------------------------------------------.
      | yydefault -- do the default action for the current state.  |
      `-----------------------------------------------------------*/
      case YYDEFAULT:
        yyn = yydefact_[yystate];
        if (yyn == 0)
          label = YYERRLAB;
        else
          label = YYREDUCE;
        break;

      /*-----------------------------.
      | yyreduce -- Do a reduction.  |
      `-----------------------------*/
      case YYREDUCE:
        yylen = yyr2_[yyn];
        label = yyaction (yyn, yystack, yylen);
        yystate = yystack.stateAt (0);
        break;

      /*--------------------------------------.
      | yyerrlab -- here on detecting error.  |
      `--------------------------------------*/
      case YYERRLAB:
        /* If not already recovering from an error, report this error.  */
        if (yyerrstatus_ == 0)
        {
          ++yynerrs_;
          yyreportSyntaxError(new Context(this, yystack, yytoken, yylloc));
        }

        yyerrloc = yylloc;
        if (yyerrstatus_ == 3)
        {
          /* If just tried and failed to reuse lookahead token after an
           * error, discard it.  */

          /* Return failure if at end of input.  */
          if (yytoken == SymbolKind.YYEOF)
          {
            label = YYABORT;
            break;
          }
          else
            yytoken = SymbolKind.YYEMPTY;
        }

        /* Else will try to reuse lookahead token after shifting the error
         * token.  */
        label = YYERRLAB1;
        break;

      /*-------------------------------------------------.
      | errorlab -- error raised explicitly by YYERROR.  |
      `-------------------------------------------------*/
      case YYERROR:
        yyerrloc = yystack.locationAt (yylen - 1);
        /* Do not reclaim the symbols of the rule which action triggered
           this YYERROR.  */
        yystack.pop (yylen);
        yylen = 0;
        yystate = yystack.stateAt (0);
        label = YYERRLAB1;
        break;

      /*-------------------------------------------------------------.
      | yyerrlab1 -- common code for both syntax error and YYERROR.  |
      `-------------------------------------------------------------*/
      case YYERRLAB1:
        yyerrstatus_ = 3;       /* Each real token shifted decrements this.  */

        // Pop stack until we find a state that shifts the error token.
        for (;;)
        {
          yyn = yypact_[yystate];
          if (!yyPactValueIsDefault(yyn))
          {
            yyn += SymbolKind.YYerror;
            if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == SymbolKind.YYerror)
            {
              yyn = yytable_[yyn];
              if (0 < yyn)
                break;
                  }
          }

          /* Pop the current state because it cannot handle the error token.  */
          if (yystack.height == 1) // de ce e 1 si nu 0
          {
            label = YYABORT;
            break;
          }

          yyerrloc = yystack.locationAt (0);
          yystack.pop ();
          yystate = yystack.stateAt (0);
          if (0 < yydebug)
            yystack.print (yyDebugStream);
        }


        /* Muck with the stack to setup for yylloc.  */
        yystack.push (0, yy_semantic_null, yylloc);
        yystack.push (0, yy_semantic_null, yyerrloc);
        yyloc = yylloc_from_stack (yystack, 2);
        yystack.pop (2);

        /* Shift the error token.  */
        yylacDiscard("error recovery");
        yy_symbol_print ("Shifting", to!SymbolKind (yystos_[yyn]), yylval, yyloc);
        yystate = yyn;
        yystack.push (yyn, yylval, yyloc);
        label = YYNEWSTATE;
        break;

      /* Accept.  */
      case YYACCEPT:
        this.push_parse_initialized = false;
        if (0 < yydebug)
          yystack.print (yyDebugStream);
        return YYACCEPT;

      /* Abort.  */
      case YYABORT:
        this.push_parse_initialized = false;
        if (0 < yydebug)
          yystack.print (yyDebugStream);
        return YYABORT;
    }
    assert(0);
  }

    bool push_parse_initialized = false;

  /**
   * (Re-)Initialize the state of the push parser.
   */
  public void push_parse_initialize()
  {

    /* Lookahead and lookahead in internal form.  */
    this.yytoken = SymbolKind.YYEMPTY;

    /* State.  */
    this.yyn = 0;
    this.yylen = 0;
    this.yystate = 0;
    destroy(this.yystack);
    this.label = YYNEWSTATE;

    destroy(this.yylacStack);
    this.yylacEstablished = false;

    /* Error handling.  */
    this.yynerrs_ = 0;

    /* The location where the error started.  */
    this.yyerrloc = Location(Position(), Position());
    this.yylloc = Location(Position(), Position());

    /* Semantic value of the lookahead.  */
    //this.yylval = YYSemanticType;

    /* Initialize the stack.  */
    yystack.push(yystate, yylval, yylloc);
    yystack.push (this.yystate, this.yylval, this.yylloc); // todo

    this.push_parse_initialized = true;
  }

  // Generate an error message.
  private final void yyreportSyntaxError(Context yyctx)
  {
    if (yyctx.getToken() != SymbolKind.YYEMPTY)
    {
      // FIXME: This method of building the message is not compatible
      // with internationalization.
      immutable int argmax = 5;
      SymbolKind[] yyarg = new SymbolKind[argmax];
      int yycount = yysyntaxErrorArguments(yyctx, yyarg, argmax);
      string res, yyformat;
      switch (yycount)
      {
        case  1:
          yyformat = YY_("syntax error, unexpected %s");
          res = format(yyformat, yyarg[0]);
         break;
        case  2:
          yyformat = YY_("syntax error, unexpected %s, expecting %s");
          res = format(yyformat, yyarg[0], yyarg[1]);
          break;
        case  3:
          yyformat = YY_("syntax error, unexpected %s, expecting %s or %s");
          res = format(yyformat, yyarg[0], yyarg[1], yyarg[2]);
          break;
        case  4:
          yyformat = YY_("syntax error, unexpected %s, expecting %s or %s or %s");
          res = format(yyformat, yyarg[0], yyarg[1], yyarg[2], yyarg[3]);
          break;
        case  5:
          yyformat = YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
          res = format(yyformat, yyarg[0], yyarg[1], yyarg[2], yyarg[3], yyarg[4]);
          break;
        default:
          res = YY_("syntax error");
          break;
      }
      yyerror(yyctx.getLocation(), res);
    }
  }


  private int yysyntaxErrorArguments(Context yyctx, SymbolKind[] yyarg, int yyargn) {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action,
         then the only way this function was invoked is if the
         default action is an error action.  In that case, don't
         check for expected tokens because there are none.
       - The only way there can be no lookahead present (in tok) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this
         state is a consistent state with a default action.  There
         might have been a previous inconsistent state, consistent
         state with a non-default action, or user semantic action
         that manipulated yychar.  (However, yychar is currently out
         of scope during semantic actions.)
       - Of course, the expected token list depends on states to
         have correct lookahead information, and it depends on the
         parser not to perform extra reductions after fetching a
         lookahead from the scanner and before detecting a syntax
         error.  Thus, state merging (from LALR or IELR) and default
         reductions corrupt the expected token list.  However, the
         list is correct for canonical LR with one exception: it
         will still contain any token that will not be accepted due
         to an error action in a later state.
    */
    int yycount = 0;
    if (yyctx.getToken() != SymbolKind.YYEMPTY)
      {
        if (yyarg !is null)
          yyarg[yycount] = yyctx.getToken();
        yycount += 1;
        yycount += yyctx.getExpectedTokens(yyarg, 1, yyargn);
      }
    return yycount;
  }



  /**
   * Information needed to get the list of expected tokens and to forge
   * a syntax error diagnostic.
   */
  public static final class Context
  {
    private Calc yyparser;
    private const(YYStack) yystack;
    private SymbolKind yytoken;
    private const(Location) yylocation;

    this(Calc parser, YYStack stack, SymbolKind kind, Location loc)
    {
        yyparser = parser;
      yystack = stack;
      yytoken = kind;
      yylocation = loc;
    }

    final SymbolKind getToken() const
    {
      return yytoken;
    }

    final const(Location) getLocation() const
    {
      return yylocation;
    }
    /**
     * Put in YYARG at most YYARGN of the expected tokens given the
     * current YYCTX, and return the number of tokens stored in YYARG.  If
     * YYARG is null, return the number of expected tokens (guaranteed to
     * be less than YYNTOKENS).
     */
    int getExpectedTokens(SymbolKind[] yyarg, int yyargn)
    {
      return getExpectedTokens(yyarg, 0, yyargn);
    }

    int getExpectedTokens(SymbolKind[] yyarg, int yyoffset, int yyargn)
    {
      int yycount = yyoffset;
      // Execute LAC once. We don't care if it is successful, we
      // only do it for the sake of debugging output.

      if (!yyparser.yylacEstablished)
        yyparser.yylacCheck(yystack, yytoken);

      for (int yyx = 0; yyx < yyntokens_; ++yyx)
        {
          SymbolKind yysym = SymbolKind(yyx);
          if (yysym != SymbolKind.YYerror
              && yysym != SymbolKind.YYUNDEF
              && yyparser.yylacCheck(yystack, yysym))
            {
              if (yyarg == null)
                yycount += 1;
              else if (yycount == yyargn)
                return 0;
              else
                yyarg[yycount++] = yysym;
            }
        }
      if (yyarg !is null && yycount == yyoffset && yyoffset < yyargn)
        yyarg[yyoffset] = SymbolKind.YYEMPTY;
      return yycount - yyoffset;
    }
  }


  /** Check the lookahead yytoken.
   * \returns  true iff the token will be eventually shifted.
   */
  bool yylacCheck(const YYStack yystack, SymbolKind yytoken)
  {
    // Logically, the yylacStack's lifetime is confined to this function.
    // Clear it, to get rid of potential left-overs from previous call.
    destroy(yylacStack);
    // Reduce until we encounter a shift and thereby accept the token.

    yycdebug("LAC: checking lookahead " ~ format("%s", yytoken) ~ ":");
    int lacTop = 0;
    while (true)
    {
      int topState = (yylacStack.length == 0
                      ? yystack.stateAt(lacTop)
                      : yylacStack[$ - 1]);
      int yyrule = yypact_[topState];
      if (yyPactValueIsDefault(yyrule)
          || (yyrule += yytoken) < 0 || yylast_ < yyrule
          || yycheck_[yyrule] != yytoken)
      {
        // Use the default action.
        yyrule = yydefact_[+topState];
        if (yyrule == 0)
        {
          yycdebugln(" Err");
          return false;
        }
      }
      else
      {
        // Use the action from yytable.
        yyrule = yytable_[yyrule];
        if (yyTableValueIsError(yyrule))
        {
          yycdebugln(" Err");
          return false;
        }
        if (0 < yyrule)
        {
          yycdebugln(" S" ~ to!string(yyrule));
          return true;
        }
        yyrule = -yyrule;
      }
      // By now we know we have to simulate a reduce.

      yycdebug(" R" ~ to!string(yyrule - 1));
      // Pop the corresponding number of values from the stack.
      {
        int yylen = yyr2_[yyrule];
        // First pop from the LAC stack as many tokens as possible.
        int lacSize = cast (int) yylacStack.length;
        if (yylen < lacSize)
        {
          yylacStack.length -= yylen;
          yylen = 0;
        }
        else if (lacSize != 0)
        {
          destroy(yylacStack);
          yylen -= lacSize;
        }
        // Only afterwards look at the main stack.
        // We simulate popping elements by incrementing lacTop.
        lacTop += yylen;
      }
      // Keep topState in sync with the updated stack.
      topState = (yylacStack.length == 0
                  ? yystack.stateAt(lacTop)
                  : yylacStack[$ - 1]);
      // Push the resulting state of the reduction.
      int state = yyLRGotoState(topState, yyr1_[yyrule]);
      yycdebug(" G" ~ to!string(state));
      yylacStack.length++;
      yylacStack[$ - 1] = state;
    }
  }

  /** Establish the initial context if no initial context currently exists.
   * \returns  true iff the token will be eventually shifted.
   */
  bool yylacEstablish(YYStack yystack, SymbolKind yytoken)
  {
  /* Establish the initial context for the current lookahead if no initial
     context is currently established.

     We define a context as a snapshot of the parser stacks.  We define
     the initial context for a lookahead as the context in which the
     parser initially examines that lookahead in order to select a
     syntactic action.  Thus, if the lookahead eventually proves
     syntactically unacceptable (possibly in a later context reached via a
     series of reductions), the initial context can be used to determine
     the exact set of tokens that would be syntactically acceptable in the
     lookahead's place.  Moreover, it is the context after which any
     further semantic actions would be erroneous because they would be
     determined by a syntactically unacceptable token.

     yylacEstablish should be invoked when a reduction is about to be
     performed in an inconsistent state (which, for the purposes of LAC,
     includes consistent states that don't know they're consistent because
     their default reductions have been disabled).

     For parse.lac=full, the implementation of yylacEstablish is as
     follows.  If no initial context is currently established for the
     current lookahead, then check if that lookahead can eventually be
     shifted if syntactic actions continue from the current context.  */
    if (yylacEstablished)
      return true;
    else
    {
        yycdebugln("LAC: initial context established for " ~ format("%s", yytoken));
        yylacEstablished = true;
        return yylacCheck(yystack, yytoken);
    }
  }

  /** Discard any previous initial lookahead context because of event.
   * \param event  the event which caused the lookahead to be discarded.
   *               Only used for debbuging output.  */
  void yylacDiscard(string event)
  {
  /* Discard any previous initial lookahead context because of Event,
     which may be a lookahead change or an invalidation of the currently
     established initial context for the current lookahead.

     The most common example of a lookahead change is a shift.  An example
     of both cases is syntax error recovery.  That is, a syntax error
     occurs when the lookahead is syntactically erroneous for the
     currently established initial context, so error recovery manipulates
     the parser stacks to try to find a new initial context in which the
     current lookahead is syntactically acceptable.  If it fails to find
     such a context, it discards the lookahead.  */
    if (yylacEstablished)
    {
      yycdebugln("LAC: initial context discarded due to " ~ event);
      yylacEstablished = false;
    }
  }

  /** The stack for LAC.
   * Logically, the yylacStack's lifetime is confined to the function
   * yylacCheck. We just store it as a member of this class to hold
   * on to the memory and to avoid frequent reallocations.
   */
  int[] yylacStack;
  /**  Whether an initial LAC context was established. */
  bool yylacEstablished;


  /**
   * Whether the given <code>yypact_</code> value indicates a defaulted state.
   * @param yyvalue   the value to check
   */
  private static bool yyPactValueIsDefault(int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  /**
   * Whether the given <code>yytable_</code> value indicates a syntax error.
   * @param yyvalue   the value to check
   */
  private static bool yyTableValueIsError(int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
  private static immutable byte yypact_ninf_ = -5;

  /* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule which
     number is the opposite.  If YYTABLE_NINF_, syntax error.  */
  private static immutable byte yytable_ninf_ = -1;

    /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
private static immutable byte[] yypact_ =
[
      17,    -4,    25,    25,    25,    -5,    -5,     3,    -5,    33,
      -5,    -5,    -5,    40,    -5,    -5,    25,    25,    25,    25,
      -5,    -5,    28,    28,    -5,    -5
];

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
private static immutable byte[] yydefact_ =
[
       0,     0,     0,     0,     0,     4,     7,     0,     2,     0,
       6,    12,    13,     0,     1,     3,     0,     0,     0,     0,
       5,    14,     8,     9,    10,    11
];

  /* YYPGOTO[NTERM-NUM].  */
private static immutable byte[] yypgoto_ =
[
      -5,    -5,     1,    -2
];

  /* YYDEFGOTO[NTERM-NUM].  */
private static immutable byte[] yydefgoto_ =
[
       0,     7,     8,     9
];

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
private static immutable byte[] yytable_ =
[
      11,    12,    13,    14,     1,    10,     2,     3,    15,     0,
       4,     0,     5,     6,    22,    23,    24,    25,     1,     0,
       2,     3,     0,     0,     4,     0,     5,     6,     2,     3,
       0,     0,     4,    18,    19,     6,    16,    17,    18,    19,
       0,     0,    20,    16,    17,    18,    19,     0,    21
];

private static immutable byte[] yycheck_ =
[
       2,     3,     4,     0,     1,     9,     3,     4,     7,    -1,
       7,    -1,     9,    10,    16,    17,    18,    19,     1,    -1,
       3,     4,    -1,    -1,     7,    -1,     9,    10,     3,     4,
      -1,    -1,     7,     5,     6,    10,     3,     4,     5,     6,
      -1,    -1,     9,     3,     4,     5,     6,    -1,     8
];

  /* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
     state STATE-NUM.  */
private static immutable byte[] yystos_ =
[
       0,     1,     3,     4,     7,     9,    10,    13,    14,    15,
       9,    15,    15,    15,     0,    14,     3,     4,     5,     6,
       9,     8,    15,    15,    15,    15
];

  /* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
private static immutable byte[] yyr1_ =
[
       0,    12,    13,    13,    14,    14,    14,    15,    15,    15,
      15,    15,    15,    15,    15
];

  /* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
private static immutable byte[] yyr2_ =
[
       0,     2,     1,     2,     1,     2,     2,     1,     3,     3,
       3,     3,     2,     2,     3
];



  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
  private static immutable byte[] yyrline_ =
  [
       0,    52,    52,    53,    57,    58,    59,    63,    64,    65,
      66,    67,    68,    69,    70
  ];

  // Report on the debug stream that the rule yyrule is going to be reduced.
  private final void yy_reduce_print (int yyrule, ref YYStack yystack)
  {
    if (yydebug == 0)
      return;

    int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    /* Print the symbols being reduced, and their result.  */
    yycdebugln (format("Reducing stack by rule %d (line %d):",
                yyrule - 1, yylno));

    /* The symbols being reduced.  */
    for (int yyi = 0; yyi < yynrhs; yyi++)
      yy_symbol_print (format("   $%d =", yyi + 1),
                       to!SymbolKind (yystos_[yystack.stateAt(yynrhs - (yyi + 1))]),
                       (yystack.valueAt ((yynrhs) - (yyi + 1))),
                       yystack.locationAt ((yynrhs) - (yyi + 1)));
  }


  private static auto yytranslate_ (int t)
  {
    return SymbolKind(t);
  }

  private static immutable int yylast_ = 48;
  private static immutable int yynnts_ = 4;
  private static immutable int yyfinal_ = 14;
  private static immutable int yyntokens_ = 12;

  private final struct YYStackElement {
    int state;
    Value value;YYLocation location;
  }

  private final struct YYStack {
    private YYStackElement[] stack = [];

    public final ulong height()
    {
      return stack.length;
    }

    public final void push (int state, Value value  , ref Location loc)
    {
      stack ~= YYStackElement(state, value, loc);
    }

    public final void pop ()
    {
      pop (1);
    }

    public final void pop (int num)
    {
      stack.length -= num;
    }

    public final int stateAt (int i) const
    {
      return stack[$-i-1].state;
    }


    public final ref Location locationAt (int i)
    {
      return stack[$-i-1].location;
    }

    public final ref Value valueAt (int i)
    {
      return stack[$-i-1].value;
    }

    // Print the state stack on the debug stream.
    public final void print (File stream)
    {
      stream.write ("Stack now");
      for (int i = 0; i < stack.length; i++)
        stream.write (" ", stack[i].state);
      stream.writeln ();
    }
  }

}
#line 73 "calc.y"

import std.range.primitives;
import std.stdio;

auto calcLexer(R)(R range)
if (isInputRange!R && is(ElementType!R : dchar))
{
  return new CalcLexer!R(range);
}

auto calcLexer(File f)
{
  import std.algorithm : map, joiner;
  import std.utf : byDchar;

  return f.byChunk(1024)        // avoid making a syscall roundtrip per char
          .map!(chunk => cast(char[]) chunk) // because byChunk returns ubyte[]
          .joiner               // combine chunks into a single virtual range of char
          .calcLexer;           // forward to other overload
}

class CalcLexer(R) : Lexer
if (isInputRange!R && is(ElementType!R : dchar))
{
  R input;

  this(R r) { input = r; }

  Location location;

  // Should be a local in main, shared with %parse-param.
  int exit_status = 0;

  void yyerror(const Location loc, string s)
  {
    exit_status = 1;
    stderr.writeln(loc.toString(), ": ", s);
  }

  Symbol yylex()
  {
    import std.uni : isWhite, isNumber;

    // Skip initial spaces
    while (!input.empty && input.front != '\n' && isWhite(input.front))
    {
      location.begin = location.end;
      location.end.column++;
      input.popFront;
    }

    if (input.empty)
      return Symbol(TokenKind.YYEOF, location);

    // Numbers.
    if (input.front.isNumber)
    {
      int ival;
      int lenChars = 0;

      import std.compiler : version_minor;
      static if (version_minor >= 95)
      {
        // from Dlang v2.095.0 onwards std.conv.parse reports
        // the number of consumed characters
        import std.typecons : Flag, Yes;
        import std.conv : parse;
        auto parsed = parse!(int, R, Yes.doCount)(input);
        ival = parsed.data;
        lenChars = cast(int) parsed.count;
      }
      else
      {
        auto copy = input;
        import std.conv : parse;
        ival = input.parse!int;
        while (!input.empty && copy.front != input.front)
        {
          lenChars++;
          copy.popFront;
        }
      }
      location.begin = location.end;
      location.end.column += lenChars;
      return Symbol(TokenKind.NUM, ival, location);
    }

    // Individual characters
    auto ch = input.front;
    input.popFront;
    location.begin = location.end;
    location.end.column++;
    switch (ch)
    {
      case '+':  return Symbol(TokenKind.PLUS, location);
      case '-':  return Symbol(TokenKind.MINUS, location);
      case '*':  return Symbol(TokenKind.STAR, location);
      case '/':  return Symbol(TokenKind.SLASH, location);
      case '(':  return Symbol(TokenKind.LPAR, location);
      case ')':  return Symbol(TokenKind.RPAR, location);
      case '\n':
      {
        location.end.line++;
        location.end.column = 1;
        return Symbol(TokenKind.EOL, location);
      }
      default: assert(0);
    }
  }
}

int main()
{
  auto l = calcLexer(stdin);
  auto p = new Calc(l);
  import core.stdc.stdlib : getenv;
  if (getenv("YYDEBUG"))
    p.setDebugLevel(1);
  p.parse();
  return l.exit_status;
}
