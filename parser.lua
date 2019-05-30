
local utf8 = require 'utf8'
utf8.charpattern = "[%z\1-\127\194-\244][\128-\191]*"

local path = require "path"
local fs = require "path.fs"

setmetatable(_G, {
	__newindex = function(_, n)
		error('attempt to write to undeclared variable ' .. n, 2)
	end,
	__index = function(_, n)
		error('attempt to read undeclared variable ' .. n, 2)
	end
})

local function Const(t)
	return setmetatable(t, {
		__tostring = function ()
			return t[1]
		end
	})
end

local IDENT    = 'Ident'
local NUMBER   = 'Number'
local STRING   = 'String'
local DATETIME = 'DateTime'
local COMMENT  = 'Comment'

local STRINGBEG = 'StringBeg'
local STRINGMID = 'StringMid'
local STRINGEND = 'StringEnd'

local EQL = 'Eql' -- =
local NEQ = 'Neq' -- <>
local LSS = 'Lss' -- <
local GTR = 'Gtr' -- >
local LEQ = 'Leq' -- <=
local GEQ = 'Geq' -- >=

local ADD = 'Add' -- +
local SUB = 'Sub' -- -
local MUL = 'Mul' -- *
local DIV = 'Div' -- /
local MOD = 'Mod' -- %

local LPAREN = '('
local RPAREN = ')'
local LBRACK = '['
local RBRACK = ']'

local TERNARY    = '?'
local COMMA      = ','
local PERIOD     = '.'
local COLON      = ':'
local SEMICOLON  = ';'

local AMPER = '&'
local LABEL = '~'
local SHARP = '#'

local SPACE = 'space'
local DIGIT = 'digit'
local ALPHA = 'alpha'
local LF = 0x0A

-- preprocessor
local _IF        = {'If', 'Если'}
local _ELSIF     = {'ElsIf', 'ИначеЕсли'}
local _ELSE      = {'Else', 'Иначе'}
local _ENDIF     = {'EndIf', 'КонецЕсли'}
local _REGION    = {'Region', 'Область'}
local _ENDREGION = {'EndRegion', 'КонецОбласти'}
local _USE       = {'Use', 'Использовать'}

-- -- directives
local ATCLIENT = {'AtClient', 'НаКлиенте'}
local ATSERVER = {'AtServer', 'НаСервере'}
-- local ATSERVERNOCONTEXT = {'AtServerNoContext', 'НаСервереБезКонтекст'}
-- local ATCLIENTATSERVERNOCONTEXT = {'AtClientAtServerNoContext', 'НаКлиентеНаСервереБезКонтекст'}
-- local ATCLIENTATSERVER = { 'AtClientAtServer', 'НаКлиентеНаСервере'}

local CLIENT = {'Client', 'Клиент'}
local MOBILEAPPCLIENT = {'MobileAppClient', 'МобильноеПриложениеКлиент'}
local MOBILEAPPSERVER = {'MobileAppServer', 'МобильноеПриложениеСервер'}
local THICKCLIENTORDINARYAPPLICATION = {'ThickClientOrdinaryApplication', 'ТолстыйКлиентОбычноеПриложение'}
local THICKCLIENTMANAGEDAPPLICATION = {'ThickClientManagedApplication', 'ТолстыйКлиентУправляемоеПриложение'}
local SERVER = {'Server', 'Сервер'}
local EXTERNALCONNECTION = {'ExternalConnection', 'ВнешнееСоединение'}
local THINCLIENT = {'ThinClient', 'ТонкийКлиент'}
local WEBCLIENT = {'WebClient', 'ВебКлиент'}

-- keywords
local IF    = {'If', 'Если'}
local THEN  = {'Then', 'Тогда'}
local ELSIF = {'ElsIf', 'ИначеЕсли'}
local ELSE  = {'Else', 'Иначе'}
local ENDIF = {'EndIf', 'КонецЕсли'}
local FOR   = {'For', 'Для'}
local EACH  = {'Each', 'Каждого'}
local IN    = {'In', 'Из'}
local TO    = {'To', 'По'}
local WHILE = {'While', 'Пока'}
local DO    = {'Do', 'Цикл'}
local ENDDO = {'EndDo', 'КонецЦикла'}
local PROCEDURE    = {'Procedure', 'Процедура'}
local ENDPROCEDURE = {'EndProcedure', 'КонецПроцедуры'}
local FUNCTION     = {'Function', 'Функция'}
local ENDFUNCTION  = {'EndFunction', 'КонецФункции'}
local VAR = {'Var', 'Перем'}
local VAL = {'Val', 'Знач'}
local RETURN   = {'Return', 'Возврат'}
local CONTINUE = {'Continue', 'Продолжить'}
local BREAK    = {'Break', 'Прервать'}
local AND = Const {'And', 'И'}
local OR  = Const {'Or', 'Или'}
local NOT = Const {'Not', 'Не'}
local TRY = {'Try', 'Попытка'}
local EXCEPT = {'Except', 'Исключение'}
local RAISE  = {'Raise', 'ВызватьИсключение'}
local ENDTRY = {'EndTry', 'КонецПопытки'}
local NEW    = {'New', 'Новый'}
local EXECUTE = {'Execute', 'Выполнить'}
local EXPORT  = {'Export', 'Экспорт'}
local GOTO  = {'Goto', 'Перейти'}
local TRUE  = Const {'True', 'Истина'}
local FALSE = Const {'False', 'Ложь'}
local UNDEFINED = Const {'Undefined', 'Неопределено'}
local NULL = Const {'Null'}

local upper = {
	['а'] = 'А', ['б'] = 'Б', ['в'] = 'В', ['г'] = 'Г', ['д'] = 'Д', ['е'] = 'Е',
	['ё'] = 'Ё', ['ж'] = 'Ж', ['з'] = 'З', ['и'] = 'И', ['й'] = 'Й', ['к'] = 'К',
	['л'] = 'Л', ['м'] = 'М', ['н'] = 'Н', ['о'] = 'О', ['п'] = 'П', ['р'] = 'Р',
	['с'] = 'С', ['т'] = 'Т', ['у'] = 'У', ['ф'] = 'Ф', ['х'] = 'Х', ['ц'] = 'Ц',
	['ч'] = 'Ч', ['ш'] = 'Ш', ['щ'] = 'Щ', ['ъ'] = 'Ъ', ['ы'] = 'Ы', ['ь'] = 'Ь',
	['э'] = 'Э', ['ю'] = 'Ю', ['я'] = 'Я', ['a'] = 'A', ['b'] = 'B', ['c'] = 'C',
	['d'] = 'D', ['e'] = 'E', ['f'] = 'F', ['g'] = 'G', ['h'] = 'H', ['i'] = 'I',
	['j'] = 'J', ['k'] = 'K', ['l'] = 'L', ['m'] = 'M', ['n'] = 'N', ['o'] = 'O',
	['p'] = 'P', ['q'] = 'Q', ['r'] = 'R', ['s'] = 'S', ['t'] = 'T', ['u'] = 'U',
	['v'] = 'V', ['w'] = 'W', ['x'] = 'X', ['y'] = 'Y', ['z'] = 'Z',
}

local function hash(list)
	local t = {}
	for _, v in ipairs(list) do
		for _, k in ipairs(v) do
			local K = k:gsub(utf8.charpattern, upper)
			t[K] = v
		end
	end
	return t
end

local keywords = hash {
    IF, THEN, ELSIF, ELSE, ENDIF, FOR, EACH, IN, TO, WHILE, DO, ENDDO,
	PROCEDURE, ENDPROCEDURE, FUNCTION, ENDFUNCTION, VAR, VAL, RETURN,
	CONTINUE, BREAK, AND, OR, NOT, TRY, EXCEPT, RAISE, ENDTRY, NEW,
	EXECUTE, EXPORT, GOTO, TRUE, FALSE, UNDEFINED, NULL
}

-- local directives = hash {
-- 	ATCLIENT, ATSERVER, ATSERVERNOCONTEXT,
-- 	ATCLIENTATSERVERNOCONTEXT, ATCLIENTATSERVER
-- }

local prepinstrs = hash {
	_IF, _ELSIF, _ELSE, _ENDIF, _REGION, _ENDREGION, _USE
}

local prepinstset = {
	[_IF] = true , [_ELSIF] = true, [_ELSE] = true, [_ENDIF] = true, [_REGION] = true, [_ENDREGION] = true, [_USE] = true
}

local prepSymbols = hash {
	ATCLIENT, ATSERVER, CLIENT, MOBILEAPPCLIENT, MOBILEAPPSERVER, THICKCLIENTORDINARYAPPLICATION,
	THICKCLIENTMANAGEDAPPLICATION, SERVER, EXTERNALCONNECTION, THINCLIENT, WEBCLIENT
}

local selectKinds = {
	Ident = 'Ident', Index = 'Index', Call = 'Call'
}

local function set(list)
	local t = {}
	for _, v in ipairs(list) do t[v] = true end
	return t
end

local basicLitNoStr = set {NUMBER, DATETIME, TRUE, FALSE, UNDEFINED, NULL}
local relOps = set {EQL, NEQ, LSS, GTR, LEQ, GEQ}
local addOps = set {ADD, SUB}
local mulOps = set {MUL, DIV, MOD}
local initOfExpr = set {
	ADD, SUB, NOT, IDENT, LPAREN, NUMBER, STRING, STRINGBEG,
	DATETIME, TERNARY, NEW, TRUE, FALSE, UNDEFINED, NULL
}

local chart

do
	local cp = utf8.codepoint
	chart = {
        [cp'_'] = ALPHA,   [cp'*'] = MUL,
        [cp'('] = LPAREN,  [cp'/'] = DIV,
        [cp')'] = RPAREN,  [cp'+'] = ADD,
        [cp'['] = LBRACK,  [cp'-'] = SUB,
        [cp']'] = RBRACK,  [cp'%'] = MOD,
        [cp'?'] = TERNARY, [cp'<'] = LSS,
        [cp'.'] = PERIOD,  [cp'>'] = GTR,
        [cp'"'] = STRING,  [cp'='] = EQL,
		[cp'|'] = STRING,  [cp"'"] = DATETIME,
		[cp':'] = COLON,   [cp';'] = SEMICOLON,
		[cp','] = COMMA,   [cp'&'] = AMPER,
		[cp'#'] = SHARP,   [cp'~'] = LABEL
    }
    for i = 0x01, 0x20 do chart[i] = SPACE end
    for i = 0x30, 0x39 do chart[i] = DIGIT end
	for k, v in pairs(upper) do
		chart[cp(k)] = ALPHA; chart[cp(v)] = ALPHA
	end
end

local function min(a, b)
	return a < b and a or b
end

local function printf(s, ...)
	print(s:format(...))
end

-------------------------------------------------------------------------------

local p = {}

function p:init(fname)
	self.line = 1
	self.endline = 1
	self.pos = 0
	self.posLF = 0
	self.begpos = 0
	self.endpos = 0
	self.chr = ''
	self.tok = nil
	self.lit = ''
	self.val = nil
	self.methods = {}
	self.unknown = {}
	self.isFunc = false
	self.allowVar = true
	self.interface = {}
	self.comments = {}
	self.directive = null
	self.vars = {}
	self.verbose = true
	self.location = true
	self.scope = nil

	self.path = path.utf8(fname)
	self.src = io.open(fname, 'r'):read('a'):sub(4)
	self.getc = utf8.codes(self.src)
	self.pos, self.chr = self.getc(self.src, 0)
end

function p:scan()

	local tok, lit
	local src, pos, chr = self.src, self.pos, self.chr
	local getc = self.getc

	if pos == nil then
		self.tok = nil
		return
	end

	self.endpos, self.endline = pos, self.line

	if self.lit:byte(-1) == LF then
		self.line = self.line + 1
	end

	repeat

		-- skip space
		while chr ~= nil and chr < 0x21 and chr > 0  or chr == 0xA0 or chr == 0x2003 do
			if chr == LF then self.line = self.line + 1; self.posLF = pos end
			pos, chr = getc(src, pos)
		end

		self.begpos = pos

		tok = chart[chr]
		lit = ''

		-- if tok == nil then
		-- 	return nil
		-- end

		if tok == ALPHA then
			local beg = pos
			repeat pos, chr = getc(src, pos); tok = chart[chr] until tok ~= ALPHA and tok ~= DIGIT
			lit = src:sub(beg, (pos or 0) - 1)
			tok = keywords[lit:gsub(utf8.charpattern, upper)] or IDENT
		elseif tok == DIGIT then
			local beg = pos
			repeat pos, chr = getc(src, pos) until chart[chr] ~= DIGIT
			if chart[chr] == PERIOD then
				repeat pos, chr = getc(src, pos) until chart[chr] ~= DIGIT
			end
			tok = NUMBER
			lit = src:sub(beg, (pos or 0) - 1)
		elseif tok == STRING then
			local beg = pos
			repeat
				repeat pos, chr = getc(src, pos) until chr == 0x22 or chr == LF or chr == nil
				if chr ~= nil then pos, chr = getc(src, pos) end
			until chr ~= 0x22
			lit = src:sub(beg, (pos or 0) - 1)
			if lit:sub(1, 1) == '"' then
				tok = lit:sub(-1, -1) == '"' and STRING or STRINGBEG
			else
				tok = lit:sub(-1, -1) == '"' and STRINGEND or STRINGMID
			end
		elseif tok == DATETIME then
			local beg = pos
			repeat pos, chr = getc(src, pos) until chr == 0x27 or chr == LF or chr == nil
			assert(chr == 0x27, "Expected '")
			lit = src:sub(beg, (pos or 0) - 1)
			pos, chr = getc(src, pos)
		elseif tok == DIV then
			pos, chr = getc(src, pos)
			if chr == 0x2F then -- '//'
				local beg = pos + 1
				repeat pos, chr = getc(src, pos) until chr == 0x0A or chr == nil
				lit = src:sub(beg, (pos or 0) - 1)
				self.comments[self.line] = lit
				tok = COMMENT
			end
		elseif tok == LSS then -- '<'
			pos, chr = getc(src, pos)
			if chr == 0x3E then -- '~='
				tok = NEQ
				pos, chr = getc(src, pos)
			elseif chr == 0x3D then -- '<='
				tok = LEQ
				pos, chr = getc(src, pos)
			end
		elseif tok == GTR then -- '>'
			pos, chr = getc(src, pos)
			if chr == 0x3D then -- '>='
				tok = GEQ
				pos, chr = getc(src, pos)
			end
		elseif tok == AMPER then
			pos, chr = getc(src, pos)
			assert(chart[chr] == ALPHA, "Expected directive")
			local beg = pos
			repeat pos, chr = getc(src, pos); tok = chart[chr] until tok ~= ALPHA and tok ~= DIGIT
			lit = src:sub(beg, (pos or 0) - 1)
			tok = AMPER
			--tok = directives[lit:gsub(utf8.charpattern, upper)]
			assert(tok, 'Unknown directive: '..lit)
		elseif tok == SHARP then
			pos, chr = getc(src, pos)
			-- skip space
			while chr and chr < 0x21 and chr > 0 do
				if chr == LF then self.line = self.line + 1; self.posLF = pos end
				pos, chr = getc(src, pos)
			end
			assert(chart[chr] == ALPHA, "Expected preprocessor instruction")
			local beg = pos
			repeat pos, chr = getc(src, pos); tok = chart[chr] until tok ~= ALPHA and tok ~= DIGIT
			lit = src:sub(beg, (pos or 0) - 1)
			tok = prepinstrs[lit:gsub(utf8.charpattern, upper)]
			assert(tok, 'Unknown directive: '..lit)
		else
			pos, chr = getc(src, pos)
		end

	until tok ~= COMMENT

	self.chr = chr; self.pos = pos; self.tok = tok; self.lit = lit

	if tok == NUMBER then
		self.val = tonumber(lit)
	elseif tok == TRUE then
		self.val = true
	elseif tok == FALSE then
		self.val = false
	elseif tok == DATETIME then
		local d = string.gsub(lit, '%D', '')..'000000'
		self.val = string.format("%s-%s-%sT%s:%s:%s",
			d:sub(1, 4), d:sub(5, 6), d:sub(7, 8),
			d:sub(9, 10), d:sub(11, 12), d:sub(13, 14)
		)
	elseif tok == NULL then
		self.val = null
	elseif tok == STRING
		or tok == STRINGBEG
		or tok == STRINGMID
		or tok == STRINGEND then
		self.val = lit:sub(2, -2)
	else
		self.val = null
	end

	return tok

end -- scan()

function p:openScope()
	self.vars = {}
	self.scope = {
		outer = self.scope,
		items = self.vars,
		auto = {}
	}
end

function p:closeScope()
	self.scope = self.scope.outer
	self.vars = self.scope.items
end

function p:findObject(name)
	local scope = self.scope
	local object = scope.items[name]
	while object == nil and scope.outer ~= nil do
		scope = scope.outer
		object = scope.items[name]
	end
	return object
end

function p:locate(pos, line)
	local place, len
	if self.location then
		if pos == nil then
			len = #self.lit
			pos = self.pos - len
		else
			len = self.endpos - pos
		end
		if line == nil then
			line = self.line
		end
		place = {
			Pos = pos,
			Len = len,
			BegLine = line,
			EndLine = self.endline,
		}

	end
	return place
end

function p:raise(note, pos, stop, posLF)
	local errortext
	if pos == nil then
		pos = min(self.pos - #self.lit, #self.src);
	end
	errortext = string.format("[ Ln: %i; Col: %i ] %s {%s}",
		self.line,
		pos - (pos == 0 and 0 or posLF or self.posLF),
		note, self.path
	);
	if stop then
		error(errortext, 2)
	else
		print(errortext);
	end
end

-- @prepEXPR

function p:parsePrepSymExpr()
	local operand
	if self.tok == IDENT then
		local symbolExist = (prepSymbols[self.lit:gsub(utf8.charpattern, upper)] ~= nil)
		operand = {
			Type = 'PrepSymExpr',
			Name = self.lit,
			Exist = symbolExist,
			Place = self:locate()
		}
		self:scan()
	else
		self:raise("Expected preprocessor symbol", nil, true)
	end
	return operand
end -- parsePrepSymExpr()

function p:parsePrepNotExpr()
	local expr
	local pos, line = self.begpos, self.line
	if self.tok == NOT then
		self:scan()
		expr = {
			Type = 'PrepNotExpr',
			Expr = self:parsePrepSymExpr(),
			Place = self:locate(pos, line)
		}
	else
		expr = self:parsePrepSymExpr()
	end
	return expr
end -- parsePrepNotExpr()

function p:parsePrepAndExpr()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parsePrepNotExpr();
	while self.tok == AND do
		operator = self.tok; self:scan()
		expr = {
			Type = 'PrepBinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parsePrepNotExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parsePrepAndExpr()

function p:parsePrepExpression()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parsePrepAndExpr();
	while self.tok == OR do
		operator = self.tok
		self:scan()
		expr = {
			Type = 'PrepBinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parsePrepAndExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parsePrepExpression()

-- @prepINST

function p:parsePrepUseInst()
	local pos, line = self.begpos, self.line
	self:scan()
	if line ~= self.line then
		self:raise("Expected string or identifier", self.endpos, true)
	end
	local path
	if self.tok == NUMBER then
		path = self.lit
		if chart[self.chr] == ALPHA then -- can be a keyword
			self:scan()
			path = path + self.lit
		end
	elseif self.tok == IDENT
		or self.tok == STRING then
		path = self.lit
	else
		self:raise("Expected string or identifier", self.endpos, true)
	end
	self:scan()
	return {
		Type = 'PrepUseInst',
		Path = path,
		Place = self:locate(pos, line)
	}
end -- parsePrepUseInst()

function p:parsePrepIfInst()
	self:scan()
	local cond = self:parsePrepExpression()
	assert(self.tok == THEN, 'expected THEN')
	self.tok = SEMICOLON -- cheat code
	return {
		Type = 'PrepIfInst',
		Cond = cond,
		Place = nil
	}
end -- parsePrepIfInst()

function p:parsePrepElsIfInst()
	self:scan()
	local cond = self:parsePrepExpression()
	assert(self.tok == THEN, 'expected THEN')
	self.tok = SEMICOLON -- cheat code
	return {
		Type = 'PrepElsIfInst',
		Cond = cond,
		Place = nil
	}
end -- parsePrepElsIfInst()

function p:parsePrepRegionInst()
	self:scan()
	assert(self.tok == IDENT, 'expected IDENT')
	local name = self.lit
	self.tok = SEMICOLON -- cheat code
	return {
		Type = 'PrepRegionInst',
		Name = name,
		Place = nil
	}
end -- parsePrepRegionInst()

-- @EXPR

function p:parseStringExpr()
	local pos, line = self.begpos, self.line
	local exprlist = {}
	local tok = self.tok
	while true do
		if tok == STRING then
			repeat
				exprlist[#exprlist+1] = {
					Type = 'BasicLitExpr',
					Kind = tok,
					Value = self.val,
					Place = self:locate()
				}
				tok = self:scan()
			until tok ~= STRING
		elseif tok == STRINGBEG then
			repeat
				exprlist[#exprlist+1] = {
					Type = 'BasicLitExpr',
					Kind = tok,
					Value = self.val,
					Place = self:locate()
				}
				tok = self:scan()
			until tok ~= STRINGMID
			if tok ~= STRINGEND then
				self:raise('Expected "', nil, true);
			end
			exprlist[#exprlist+1] = {
				Type = 'BasicLitExpr',
				Kind = tok,
				Value = self.val,
				Place = self:locate()
			}
			tok = self:scan()
		else
			break;
		end
	end
	return {
		Type = 'StringExpr',
		List = exprlist,
		Place = self:locate(pos, line)
	}
end -- parseStringExpr()

function p:parseArguments()
	local exprList = {}
	while true do
		if initOfExpr[self.tok] then
			exprList[#exprList+1] = self:parseExpression()
		else
			exprList[#exprList+1] = null
		end
		if self.tok == COMMA then
			self:scan()
		else
			break
		end
	end
	return exprList;
end -- parseArguments()

function p:parseSelectExpr()
	local value, selectExpr
	local pos, line = self.begpos, self.line
	local tok = self.tok
	if tok == PERIOD then
		self:scan()
		if keywords[self.lit:gsub(utf8.charpattern, upper)] == nil then
			assert(self.tok == IDENT, 'expected IDENT')
		end
		value = self.lit
		self:scan()
		selectExpr = {
			Type = 'SelectExpr',
			Kind = selectKinds.Ident,
			Value = value,
			Place = self:locate(pos, line)
		}
	elseif tok == LBRACK then
		tok = self:scan()
		if tok == RBRACK then
			self:raise("Expected expression", pos, true)
		end
		value = self:parseExpression()
		assert(self.tok == RBRACK, 'expected RBRACK')
		self:scan()
		selectExpr = {
			Type = 'SelectExpr',
			Kind = selectKinds.Index,
			Value = value,
			Place = self:locate(pos, line)
		}
	elseif tok == LPAREN then
		tok = self:scan()
		if tok == RPAREN then
			value = {}
		else
			value = self:parseArguments()
		end
		assert(self.tok == RPAREN, 'expected RPAREN '..self.line..self.path)
		self:scan()
		selectExpr = {
			Type = 'SelectExpr',
			Kind = selectKinds.Call,
			Value = value,
			Place = self:locate(pos, line)
		}
	end
	return selectExpr
end -- parseSelectExpr()

function p:parseDesigExpr(allowNewVar)
	local object, list, kind, newvar
	local pos, line = self.begpos, self.line
	local name = self.lit
	self:scan()
	local selectExpr = self:parseSelectExpr()
	if selectExpr == nil then
		object = self:findObject(name)
		list = {}
	else
		allowNewVar = nil
		kind = selectExpr.Kind
		if kind == "Call" then
			object = self.methods[name]
			if object == nil then
				object = self.unknown[name]
				if object == nil then
					object = {
						Type = 'Unknown',
						Name = name
					}
					self.unknown[name] = object
				end
			end
		else
			object = self:findObject(name)
		end
		list = {}
		list[#list+1] = selectExpr
		selectExpr = self:parseSelectExpr()
		while selectExpr ~= nil do
			kind = selectExpr.Kind
			list[#list+1] = selectExpr
			selectExpr = self:parseSelectExpr()
		end
	end
	if object == nil then
		if allowNewVar then
			object = {
				Type = 'VarLoc',
				Name = name,
				Auto = true
			}
			newvar = object
		else
			object = {
				Type = 'Unknown',
				Name = name
			}
			if self.verbose then
				self:raise(string.format("Undeclared identifier `%s`", name), pos)
			end
		end
	end
	return {
		Type = 'DesigExpr',
		Object = object,
		Select = list,
		Call = (kind == selectKinds.Call),
		Place = self:locate(pos, line)
	}, newvar
end -- parseDesigExpr()

function p:parseParenExpr()
	local pos, line = self.begpos, self.line
	self:scan()
	local expr = self:parseExpression()
	assert(self.tok == RPAREN, 'expected RPAREN'); self:scan()
	return {
		Type = 'ParenExpr',
		Expr = expr,
		Place = self:locate(pos, line)
	}
end -- parseParenExpr()

function p:parseNewExpr()
	local name, args
	local pos, line = self.begpos, self.line
	local tok = self:scan()
	if tok == IDENT then
		name = self.lit
		args = {}
		tok = self:scan()
	end
	if tok == LPAREN then
		tok = self:scan()
		if tok ~= RPAREN then
			args = self:parseArguments()
			assert(self.tok == RPAREN, 'expected RPAREN')
		end
		self:scan()
	end
	if name == nil and args == nil then
		self:raise('Expected constructor', self.endpos, true)
	end
	return {
		Type = 'NewExpr',
		Name = name,
		Args = args,
		Place = self:locate(pos, line)
	}
end -- parseNewExpr()

function p:parseTernaryExpr()
	local cond, thenPart, elsePart
	local selectList, selectExpr
	local pos, line = self.begpos, self.line
	self:scan()
	assert(self.tok == LPAREN, 'expected LPAREN'); self:scan()
	cond = self:parseExpression()
	assert(self.tok == COMMA, 'expected COMMA'); self:scan()
	thenPart = self:parseExpression()
	assert(self.tok == COMMA, 'expected COMMA'); self:scan()
	elsePart = self:parseExpression()
	assert(self.tok == RPAREN, 'expected RPAREN'); self:scan()
	if self.tok == PERIOD then
		selectList = {}
		selectExpr = self:parseSelectExpr()
		while selectExpr ~= nil do
			selectList[#selectList+1] = selectExpr
			selectExpr = self:parseSelectExpr()
		end
	else
		selectList = {}
	end
	return {
		Type = 'TernaryExpr',
		Cond = cond,
		Then = thenPart,
		Else = elsePart,
		Select = selectList,
		Place = self:locate(pos, line)
	}
end -- parseTernaryExpr()

function p:parseOperand()
	local operand
	local tok = self.tok
	if tok == STRING or tok == STRINGBEG then
		operand = self:parseStringExpr()
	elseif basicLitNoStr[tok] then
		operand = {
			Type = 'BasicLitExpr',
			Kind = tostring(tok),
			Value = self.val,
			Place = self:locate()
		}
		self:scan()
	elseif tok == IDENT then
		operand = self:parseDesigExpr()
	elseif tok == LPAREN then
		operand = self:parseParenExpr()
	elseif tok == NEW then
		operand = self:parseNewExpr()
	elseif tok == TERNARY then
		operand = self:parseTernaryExpr()
	else
		self:raise('Expected operand', nil, true)
	end
	return operand
end -- parseOperand()

function p:parseUnaryExpr()
	local expr, operator
	local pos, line = self.begpos, self.line
	if addOps[self.tok] then
		operator = self.tok; self:scan()
		expr = {
			Type = 'UnaryExpr',
			Operator = tostring(operator),
			Operand = self:parseOperand(),
			Place = self:locate(pos, line)
		}
	elseif self.tok then
		expr = self:parseOperand()
	else
		expr = null
	end
	return expr
end -- parseUnaryExpr()

function p:parseMulExpr()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parseUnaryExpr()
	while mulOps[self.tok] do
		operator = self.tok; self:scan()
		expr = {
			Type = 'BinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parseUnaryExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parseMulExpr()

function p:parseAddExpr()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parseMulExpr()
	while addOps[self.tok] do
		operator = self.tok; self:scan()
		expr = {
			Type = 'BinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parseMulExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parseAddExpr()

function p:parseRelExpr()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parseAddExpr()
	while relOps[self.tok] do
		operator = self.tok; self:scan()
		expr = {
			Type = 'BinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parseAddExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parseRelExpr()

function p:parseNotExpr()
	local expr
	local pos, line = self.begpos, self.line
	if self.tok == NOT then
		self:scan()
		expr = {
			Type = 'NotExpr',
			Expr = self:parseRelExpr(),
			Place = self:locate(pos, line)
		}
	else
		expr = self:parseRelExpr()
	end
	return expr
end -- parseNotExpr()

function p:parseAndExpr()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parseNotExpr()
	while self.tok == AND do
		operator = self.tok; self:scan()
		expr = {
			Type = 'BinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parseNotExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parseAndExpr()

function p:parseExpression()
	local expr, operator
	local pos, line = self.begpos, self.line
	expr = self:parseAndExpr()
	while self.tok == OR do
		operator = self.tok; self:scan()
		expr = {
			Type = 'BinaryExpr',
			Left = expr,
			Operator = tostring(operator),
			Right = self:parseAndExpr(),
			Place = self:locate(pos, line)
		}
	end
	return expr
end -- parseExpression()


-- @STMT

function p:parseAssignOrCallStmt()
	local left, newvar = self:parseDesigExpr(true)
	local stmt
	if left.Call then
		stmt = {
			Type = 'CallStmt',
			Desig = left,
			Place = nil
		}
	else
		assert(self.tok == EQL, 'expected EQL')
		self:scan()
		local right = self:parseExpression()
		if newvar ~= nil then
			self.vars[newvar.Name] = newvar
			self.scope.auto[#self.scope.auto+1] = newvar
		end
		stmt = {
			Type = 'AssignStmt',
			Left = left,
			Right = right,
			Place = nil
		}
	end
	return stmt
end -- parseAssignOrCallStmt()

function p:parseIfStmt()
	local cond, thenPart
	local elsIfPart, elsePart = null, null
	local elsIfCond, elsIfThen
	self:scan()
	cond = self:parseExpression()
	assert(self.tok == THEN, 'expected THEN')
	self:scan()
	thenPart = self:parseStatements()
	local tok = self.tok
	if tok == ELSIF then
		elsIfPart = {}
		while tok == ELSIF do
			local pos, line = self.begpos, self.line
			self:scan()
			elsIfCond = self:parseExpression()
			assert(self.tok == THEN, 'expected THEN')
			self:scan()
			elsIfThen = self:parseStatements()
			elsIfPart[#elsIfPart+1] = {
				Type = 'ElsIfStmt',
				Cond = elsIfCond,
				Then = elsIfThen,
				Place = self:locate(pos, line)
			}
			tok = self.tok
		end
	end
	if tok == ELSE then
		self:scan()
		elsePart = self:parseStatements()
	end
	assert(self.tok == ENDIF, 'expected ENDIF')
	self:scan()
	return {
		Type = 'IfStmt',
		Cond = cond,
		Then = thenPart,
		ElsIf = elsIfPart,
		Else = elsePart,
		Place = nil
	}
end -- parseIfStmt()

function p:parseTryStmt()
	self:scan()
	local tryPart = self:parseStatements()
	assert(self.tok == EXCEPT, 'expected EXCEPT'); self:scan()
	local exceptPart = self:parseStatements()
	assert(self.tok == ENDTRY, 'expected ENDTRY'); self:scan()
	return {
		Type = 'TryStmt',
		Try = tryPart,
		Except = exceptPart,
		Place = nil
	}
end -- parseTryStmt()

function p:parseWhileStmt()
	self:scan()
	local cond = self:parseExpression()
	assert(self.tok == DO, 'expected DO'); self:scan()
	local statements = self:parseStatements()
	assert(self.tok == ENDDO, 'expected ENDDO'); self:scan()
	return {
		Type = 'WhileStmt',
		Cond = cond,
		Body = statements,
		Place = nil
	}
end -- parseWhileStmt()

function p:parseForEachStmt()
	self:scan()
	assert(self.tok == IDENT, 'expected IDENT')
	local varpos = self.begpos
	local desigExpr, newvar = self:parseDesigExpr(true)
	if desigExpr.Call then
		self:raise("Expected variable", varpos, true)
	end
	assert(self.tok == IN, 'expected IN')
	self:scan()
	local collection = self:parseExpression()
	if newvar ~= nil then
		self.vars[newvar.Name] = newvar
		self.scope.auto[#self.scope.auto+1] = newvar
	end
	assert(self.tok == DO, 'expected DO')
	self:scan()
	local statements = self:parseStatements()
	assert(self.tok == ENDDO, 'expected ENDDO')
	self:scan()
	return {
		Type = 'ForEachStmt',
		Desig = desigExpr,
		In = collection,
		Body = statements,
		Place = nil
	}
end -- parseForEachStmt()

function p:parseForStmt()
	assert(self.tok == IDENT, 'expected IDENT')
	local varpos = self.begpos
	local desigExpr, newvar = self:parseDesigExpr(true)
	if desigExpr.call then
		self:raise("Expected variable", varpos, true)
	end
	assert(self.tok == EQL, 'expected EQL')
	self:scan()
	local from = self:parseExpression()
	assert(self.tok == TO, 'expected TO')
	self:scan()
	local to = self:parseExpression()
	if newvar ~= nil then
		self.vars[newvar.Name] = newvar
		self.scope.auto[#self.scope.auto+1] = newvar
	end
	assert(self.tok == DO, 'expected DO')
	self:scan()
	local statements = self:parseStatements()
	assert(self.tok == ENDDO, 'expected ENDDO')
	self:scan()
	return {
		Type = 'ForStmt',
		Desig = desigExpr,
		From = from,
		To = to,
		Body = statements,
		Place = nil
	}
end -- parseForStmt()

function p:parseReturnStmt()
	local pos, line = self.begpos, self.line
	local expr
	self:scan()
	if self.isFunc then
		expr = self:parseExpression()
	end
	return {
		Type = 'ReturnStmt',
		Expr = expr,
		Place = self:locate(pos, line)
	}
end -- parseReturnStmt()

function p:parseRaiseStmt()
	local expr
	self:scan()
	if initOfExpr[self.tok] then
		expr = self:parseExpression()
	end
	return {
		Type = 'RaiseStmt',
		Expr = expr,
		Place = nil
	}
end -- parseRaiseStmt()

function p:parseExecuteStmt()
	self:scan()
	return {
		Type = 'ExecuteStmt',
		Expr = self:parseExpression(),
		Place = nil
	}
end -- parseExecuteStmt()

function p:parseGotoStmt()
	self:scan()
	assert(self.tok == LABEL, 'expected LABEL')
	local label = self.lit
	self:scan()
	return {
		Type = 'GotoStmt',
		Label = label,
		Place = nil
	}
end -- parseGotoStmt()

function p:parseStmt()
	local pos, line = self.begpos, self.line
	local stmt
	local tok = self.tok
	if tok == IDENT then
		stmt = self:parseAssignOrCallStmt()
	elseif tok == IF then
		stmt = self:parseIfStmt()
	elseif tok == TRY then
		stmt = self:parseTryStmt()
	elseif tok == WHILE then
		stmt = self:parseWhileStmt()
	elseif tok == FOR then
		if self:scan() == EACH then
			stmt = self:parseForEachStmt()
		else
			stmt = self:parseForStmt()
		end
	elseif tok == RETURN then
		stmt = self:parseReturnStmt()
	elseif tok == BREAK then
		self:scan()
		stmt = {
			Type = 'BreakStmt',
			Place = nil
		}
	elseif tok == CONTINUE then
		self:scan()
		stmt = {
			Type = 'ContinueStmt',
			Place = nil
		}
	elseif tok == RAISE then
		stmt = self:parseRaiseStmt()
	elseif tok == EXECUTE then
		stmt = self:parseExecuteStmt()
	elseif tok == GOTO then
		stmt = self:parseGotoStmt()
	elseif tok == LABEL then
		stmt = {
			Type = 'LabelStmt',
			Label = self.lit,
			Place = nil
		}
		self:scan()
		assert(self.tok == COLON, 'expected COLON')
		self.tok = SEMICOLON -- cheat code
	elseif tok == _REGION then
		stmt = self:parsePrepRegionInst()
	elseif tok == _ENDREGION then
		stmt = {Type = 'PrepEndRegionInst'}
		self.tok = SEMICOLON -- cheat code
	elseif tok == _IF then
		stmt = self:parsePrepIfInst()
	elseif tok == _ELSIF then
		stmt = self:parsePrepElsIfInst()
	elseif tok == _ELSE then
		stmt = {Type = 'PrepElseInst'}
		self.tok = SEMICOLON -- cheat code
	elseif tok == _ENDIF then
		stmt = {Type = 'PrepEndIfInst'}
		self.tok = SEMICOLON -- cheat code
	elseif tok == SEMICOLON then
		stmt = nil -- NOP
	end
	if stmt ~= nil then
		stmt.Place = self:locate(pos, line)
	end
	return stmt
end -- parseStmt()

function p:parseStatements()
	local statements, stmt
	statements = {}
	stmt = self:parseStmt()
	if stmt ~= nil then
		statements[#statements+1] = stmt
	end
	while true do
		if self.tok == SEMICOLON then
			self:scan()
		elseif prepinstset[self.tok] == nil then
			break
		end
		stmt = self:parseStmt()
		if stmt ~= nil then
			statements[#statements+1] = stmt
		end
	end
	return statements
end -- parseStatements()


-- @DECL

function p:parseVarMod()
	local name, object, export
	local pos = self.begpos
	assert(self.tok == IDENT, 'expected IDENT')
	name = self.lit
	self:scan()
	if self.tok == EXPORT then
		export = true
		self:scan()
	else
		export = false
	end
	object = {
		Type = 'VarMod',
		Name = name,
		Directive = self.directive,
		Export = export
	}
	if export then
		self.interface[#self.interface+1] = object
	end
	if self.vars[name] then
		self:raise("Identifier already declared", pos, true)
	end
	self.vars[name] = object
	return object
end -- parseVarMod()

function p:parseVarModDecl()
	local pos, line = self.begpos, self.line
	self:scan()
	local varList = {}
	varList[#varList+1] = self:parseVarMod()
	while self.tok == COMMA do
		self:scan()
		varList[#varList+1] = self:parseVarMod()
	end
	local decl = {
		Type = 'VarModDecl',
		Directive = self.directive,
		List = varList,
		Place = self:locate(pos, line)
	}
	assert(self.tok == SEMICOLON, 'expected SEMICOLON')
	self:scan()
	while self.tok == SEMICOLON do
		self:scan()
	end
	return decl
end -- parseVarModDecl()

function p:parseVarLoc()
	local name, object
	local pos = self.begpos
	assert(self.tok == IDENT, 'expected IDENT')
	name = self.lit
	object = {
		Type = 'VarLoc',
		Name = name,
		Auto = false
	}
	if self.vars[name] then
		self:raise("Identifier already declared", pos, true)
	end
	self.vars[name] = object
	self:scan()
	return object
end -- parseVarLoc()

function p:parseVarLocDecl()
	local pos, line = self.begpos, self.line
	self:scan()
	local varList = {}
	varList[#varList+1] = self:parseVarLoc()
	while self.tok == COMMA do
		self:scan()
		varList[#varList+1] = self:parseVarLoc()
	end
	return {
		Type = 'VarLocDecl',
		List = varList,
		Place = self:locate(pos, line)
	}
end -- parseVarLocDecl()

function p:parseVarDecls()
	local decls = {}
	while self.tok == VAR do
		decls[#decls+1] = self:parseVarLocDecl()
		assert(self.tok == SEMICOLON, 'expected SEMICOLON')
		self:scan()
	end
	return decls
end -- parseVarDecls()

function p:parseParameter()
	local name, object, byVal
	local pos = self.begpos
	if self.tok == VAL then
		byVal = true
		self:scan()
	else
		byVal = false
	end
	assert(self.tok == IDENT, 'expected IDENT')
	name = self.lit
	self:scan()
	if self.tok == EQL then
		self:scan()
		object = {
			Type = 'Param',
			Name = name,
			ByVal = byVal,
			Value = self:parseUnaryExpr()
		}
	else
		object = {
			Type = 'Param',
			Name = name,
			ByVal = byVal,
			Value = null
		}
	end
	if self.vars[name] then
		self:raise("Identifier already declared", pos, true)
	end
	self.vars[name] = object
	return object
end -- parseParameter()

function p:parseParamList()
	local paramList
	assert(self.tok == LPAREN, 'expected LPAREN')
	self:scan()
	if self.tok == RPAREN then
		paramList = {}
	else
		paramList = {}
		paramList[#paramList+1] = self:parseParameter()
		while self.tok == COMMA do
			self:scan()
			paramList[#paramList+1] = self:parseParameter()
		end
	end
	assert(self.tok == RPAREN, 'expected RPAREN')
	self:scan()
	return paramList
end -- parseParamList()

function p:parseFuncDecl()
	local object, name, decls, paramlist, export, statements, auto
	local pos, line, posLF = self.begpos, self.line, self.posLF
	export = false
	self:scan()
	assert(self.tok == IDENT, 'expected IDENT')
	name = self.lit
	self:scan()
	self:openScope()
	paramlist = self:parseParamList()
	if self.tok == EXPORT then
		export = true
		self:scan()
	end
	object = self.unknown[name]
	if object then
		object.Type = 'Func'
		object.Directive = self.directive
		object.Params = paramlist
		object.Export = export
		self.unknown[name] = nil
	else
		object = {
			Type = 'Func',
			Name = name,
			Directive = self.directive,
			Params = paramlist,
			Export = export
		}
	end
	if self.methods[name] then
		self:raise("Method already declared", pos, true, posLF)
	end
	self.methods[name] = object
	if export then
		self.interface[#self.interface+1] = object
	end
	decls = self:parseVarDecls()
	self.isFunc = true
	statements = self:parseStatements()
	self.isFunc = false
	assert(self.tok == ENDFUNCTION, 'expected ENDFUNCTION')
	auto = {}
	for _, varObj in ipairs(self.scope.auto) do
		auto[#auto+1] = varObj
	end
	self:closeScope()
	self:scan()
	return {
		Type = 'FuncDecl',
		Object = object,
		Decls = decls,
		Auto = auto,
		Body = statements,
		Place = self:locate(pos, line)
	}
end -- parseFuncDecl()

function p:parseProcDecl()
	local object, name, decls, paramlist, export, statements, auto
	local pos, line = self.begpos, self.line
	export = false
	self:scan()
	assert(self.tok == IDENT, 'expected IDENT')
	name = self.lit
	self:scan()
	self:openScope()
	paramlist = self:parseParamList()
	if self.tok == EXPORT then
		export = true
		self:scan()
	end
	object = self.unknown[name]
	if object then
		object.Type = 'Proc'
		object.Directive = self.directive
		object.Params = paramlist
		object.Export = export
		self.unknown[name] = nil
	else
		object = {
			Type = 'Proc',
			Name = name,
			Directive = self.directive,
			Params = paramlist,
			Export = export
		}
	end
	if self.methods[name] then
		self:raise("Method already declared", pos, true)
	end
	self.methods[name] = object
	if export then
		self.interface[#self.interface+1] = object
	end
	decls = self:parseVarDecls()
	statements = self:parseStatements()
	assert(self.tok == ENDPROCEDURE, 'expected ENDPROCEDURE')
	auto = {}
	for _, varObj in ipairs(self.scope.auto) do
		auto[#auto+1] = varObj
	end
	self:closeScope()
	self:scan()
	return {
		Type = 'ProcDecl',
		Object = object,
		Decls = decls,
		Auto = auto,
		Body = statements,
		Place = self:locate(pos, line)
	}
end -- parseProcDecl()

function p:parseModDecls()
	local decls = {}
	local tok = self.tok
	while tok == AMPER do
		self.directive = self.lit
		tok = self:scan()
	end
	local inst
	while true do
		local pos, line = self.begpos, self.line
		if tok == VAR and self.allowVar then
			decls[#decls+1] = self:parseVarModDecl()
		elseif tok == FUNCTION then
			decls[#decls+1] = self:parseFuncDecl()
			self.allowVar = false
		elseif tok == PROCEDURE then
			decls[#decls+1] = self:parseProcDecl()
			self.allowVar = false
		elseif tok == _REGION then
			inst = self:parsePrepRegionInst()
			self:scan()
			inst.place = self:locate(pos, line)
			decls[#decls+1] = inst
		elseif tok == _ENDREGION then
			self:scan()
			decls[#decls+1] = {
				Type = 'PrepEndRegionInst',
				Place = self:locate(pos, line)
			}
		elseif tok == _IF then
			inst = self:parsePrepIfInst()
			self:scan()
			inst.place = self:locate(pos, line)
			decls[#decls+1] = inst
		elseif tok == _ELSIF then
			inst = self:parsePrepElsIfInst()
			self:scan()
			inst.place = self:locate(pos, line)
			decls[#decls+1] = inst
		elseif tok == _ELSE then
			self:scan()
			decls[#decls+1] = {
				Type = 'PrepEndRegionInst',
				Place = self:locate(pos, line)
			}
		elseif tok == _ENDIF then
			self:scan()
			decls[#decls+1] = {
				Type = 'PrepEndRegionInst',
				Place = self:locate(pos, line)
			}
		elseif tok == _USE then
			decls[#decls+1] = self:parsePrepUseInst()
		else
			break
		end
		tok = self.tok
		self.directive = null
		while tok == AMPER do
			self.directive = self.lit
			tok = self:scan()
		end
	end
	return decls
end -- parseModDecls()

function p:parseModule()
	self:openScope()
	self:scan()
	local module = {
		Type = 'Module',
		Decls = self:parseModDecls(),
		Body = self:parseStatements(),
		Auto = {},
		Interface = self.interface,
		Comments = self.comments
	}
	table.move(self.scope.auto, 1, #self.scope.auto, 1, module.Auto)
	if self.verbose then
		for key in pairs(self.unknown) do
			printf("Undeclared method `%s`", key)
		end
	end
	assert(self.tok == nil, 'expected nil')
	return module
end -- parseModule()

local lines = 0

-- [console]::OutputEncoding = [System.Text.Encoding]::UTF8

for fname, type in fs.walk(arg[1]) do
	if type == "file" and fs.fnmatch(fname, "*.bsl") then
        local p1 = setmetatable(_G, {
			__index = p
		})
		p1:init(fname); p1.verbose = false
		local r, m = pcall(p1.parseModule, p1)
		if not r then
			print(m)
		else
			lines = lines + p1.line
		end
    end
end

print(lines, "lines")
print('done', os.clock(), 'sec.')
