-- author: http://github.com/olueiro
-- MIT licensed

local VERSION = "0.2.3"

local TEXT = 1
local CODE = 2
local HEADING = 4
local PASSAGE = 5
local MOVE = 6

local STARTED = 1
local SELECTED = 2
local QUERIED = 3
local REPLIED = 4
local COMPLETE = 5

local localization = {
	["en"] = {
		language = "English",
		author = "olueiro",
		code = {
      user = "user",
      reply = "reply",
      replies = "replies",
			write = "write",
			move = "move",
      visit = "play",
      set = "set",
			get = "get"
		},
		locales = {
      ["main"] = "main",
      ["File already included"] = "File already included",
      ["Unable to include file (%s)"] = "Unable to include file (%s)",
      ["Title not specified"] = "Title not specified",
      ["Syntax error in file '%s', line %s (%s)"] = "Syntax error in file '%s', code start at line %s (%s)",
      ["No environment found for user %s"] = "No environment found for user %s",
      ["User not specified"] = "User not specified",
      ["Section not found (%s)"] = "Section not found (%s)",
      ["Section in execution (%s)"] = "Section in execution (%s)",
      ["Passage '%s' not found in section '%s'"] = "Passage '%s' not found in section '%s'",
      ["Event not specified"] = "Event not specified",
      ["Invalid callback"] = "Invalid callback",
      ["Invalid reply"] = "Invalid reply",
      ["Unexpected reply"] = "Unexpected reply",
      ["Code error (%s)"] = "Code error (%s)"
		}
	}
}

local function locale(self, message, ...)
	local localization = localization[self.localization]
	if localization and localization.locales[message] then
		return string.format(localization.locales[message], ...)
	end
	return string.format(message, ...)
end

local configuration = {
	path = "",
	localization = "en",
	tags = {
		{"#{", "}", "return tostring(%s)"}
	},
	free = "?",
	handlers = {
		code_tester = function(block)
      local chunck, message
      if loadstring then
        chunck, message = loadstring(block.content)
      else
        chunck, message = load(block.content)
      end
      if chunck then
        return true, nil
      end
      return false, message
		end,
		code_loader = function(parent, user, block)
      if not parent.sandboxes[user] then
        return false, locale(parent.options, "No environment found for user %s", user)
      end
      local sandbox
      if loadstring and setfenv then
        -- Lua 5.1
        sandbox = function(environment)
          local chunck, message = loadstring(block.content)
          setfenv(chunck, environment)
          if chunck then
            return true, chunck
          else
            return false, message
          end
        end
      else
        -- Lua 5.2+, lua.vm.js, LuaJIT
        sandbox = function(environment)
          local chunck, message = load(block.content, nil, nil, environment)
          if chunck then
            return true, chunck
          else
            return false, message
          end
        end
      end
      return sandbox(parent.sandboxes[user])
    end,
    importer = function(path)
      local file, message = io.open(path, "rb")
      if file then
        local content = file:read("*a")
        file:close()
        return content, message
      end
      return file, message
    end
	},
	events = {
		["error"] = function() --(message, details)
		end,
  	["notice"] = function() --(user, message, details)
		end,
		["load"] = function(parent, user)
			return parent.data[user] or false
		end,
		["save"] = function(parent, user, data)
			parent.data[user] = data
			return true
		end,
		["section-open"] = function(parent, user, data) --(parent, user, data)
       print(data.title)
		end,
		["section-close"] = function() --(parent, user, data)
		end,
  	["play"] = function() --(parent, user)
    end,
		["close"] = function() --(parent, user)
		end,
		["query"] = function() --(parent, user, data)
      print("query")
    end,
    ["text"] = function(parent, user, data) --(parent, user, data)
      print(data.content)
		end,
	},
	state = {
		data = {},
		historic = {}
	}
}

local function table_unpack(t)
	return (table.unpack or unpack)(t or {})
end

local function table_merge(t1, t2)
	for key, value in pairs(t2) do
		if type(value) == "table" and type(t1[key] or false) == "table" then
			table_merge(t1[key], value)
		else
			t1[key] = value
		end
	end
	return t1
end

local whitelist = {
  ipairs = ipairs,
  next = next,
  pairs = pairs,
  pcall = pcall,
  tonumber = tonumber,
  tostring = tostring,
  type = type,
  unpack = table_unpack,
  coroutine = {
    create = coroutine.create,
    resume = coroutine.resume,
    running = coroutine.running,
    status = coroutine.status,
    wrap = coroutine.wrap
  },
  string = {
    byte = string.byte,
    char = string.char,
    find = string.find,
    format = string.format,
    gmatch = string.gmatch,
    gsub = string.gsub,
    len = string.len,
    lower = string.lower,
    match = string.match,
    rep = string.rep, -- !!!
    reverse = string.reverse,
    sub = string.sub,
    upper = string.upper
  },
  table = {
    insert = table.insert,
    maxn = table.maxn,
    remove = table.remove,
    sort = table.sort,
    unpack = table_unpack
  },
  math = {
    abs = math.abs,
    acos = math.acos,
    asin = math.asin,
    atan = math.atan,
    atan2 = math.atan2,
    ceil = math.ceil,
    cos = math.cos,
    cosh = math.cosh,
    deg = math.deg,
    exp = math.exp,
    floor = math.floor,
    fmod = math.fmod,
    frexp = math.frexp,
    huge = math.huge,
    ldexp = math.ldexp,
    log = math.log,
    log10 = math.log10,
    max = math.max,
    min = math.min,
    modf = math.modf,
    pi = math.pi,
    pow = math.pow,
    rad = math.rad,
    random = math.random,
    sin = math.sin,
    sinh = math.sinh,
    sqrt = math.sqrt,
    tan = math.tan,
    tanh = math.tanh
  },
  os = {
    clock = os.clock,
    difftime = os.difftime,
    time = os.time
  }
}

local function bind(text, options)

	local blocks = {}

	options = table_merge({
		included = false,
		file = "",
		caption = "",
		localization = configuration.localization,
		path = configuration.path,
    handlers = {
      code_tester = configuration.handlers.code_tester,
      code_loader = configuration.handlers.code_loader,
      importer = configuration.handlers.importer
    }
	}, options or {})

	if text:sub(-1) ~= "\n" then
		text = text .. "\n"
	end

	local index = 0
  local last
	local errors = {}
	local content = {}
	local code = {}
	local block = TEXT

	local function insert(current)
		if current == TEXT then
			if #content > 0 then
				local data = table.concat(content, "\n")
				data = string.gsub(data, "^(%s*)", "")
				data = string.gsub(data, "(%s*)$", "")
				data = string.gsub(data, "(\n*)$", "")
        if data ~= "" then
          table.insert(blocks, {
            block = TEXT,
            content = data,
            file = options.file,
            line = index - #content,
            active = true
          })
        end
				content = {}
				code = {}
			end
		elseif current == CODE then
			local data = table.concat(content, "\n")
      local code_block = {
				block = CODE,
				content = data,
				caption = code.caption,
        decorator = code.decorator,
				file = options.file,
				line = index - #content,
        active = true
			}
			local status, message = options.handlers.code_tester(table_merge({}, code_block))
			if not status then
				table.insert(errors, {
					message = locale(options, "Syntax error in file '%s', line %s (%s)", options.file == "" and locale(options, "main") or options.file, index - #content, message),
					file = options.file,
					line = index - #content
				})
			end
			table.insert(blocks, code_block)
			content = {}
			code = {}
		end
	end

	for line in text:gmatch("(.-)\n") do
		index = index + 1

		if block == TEXT and string.find(line, "^<.->.-$") then
			insert(block)
			local main, caption = string.match(line, "^<(.-)>(.-)$")
			local historic = { table_unpack(options.historic) }
			table.insert(historic, options.file)
			local valid = true
			for i = 1, #historic do
				if historic[i] == main then
					table.insert(errors, {
						message = locale(options, "File already included"),
						file = options.file,
						line = index
					})
					valid = false
					break
				end
			end
			if valid then
				local content, message = options.handlers.importer(options.path .. main)
				if content then
					local file_blocks, file_errors = bind(content, {
						included = true,
						caption = caption,
						historic = historic,
						file = options.path .. main
					})
					for i = 1, #file_blocks do
						table.insert(blocks, file_blocks[i])
					end
					for i = 1, #file_errors do
						table.insert(errors, file_errors[i])
					end
				else
					table.insert(errors, {
						message = locale(options, "Unable to include file (%s)", message),
						file = options.file,
						line = index
					})
				end
			end
			last = nil
		elseif not code.block and string.find(line, "^#+[^#]*#*.-$") then
			insert(block)
			local decorator, main, caption = string.match(line, "^(#+)([^#]*)#*(.-)$")
			table.insert(blocks, {
				block = HEADING,
				title = main,
				caption = caption,
				decorator = decorator,
				file = options.file,
				line = index,
        active = true
			})
			last = nil
		elseif not code.block and string.find(line, "^%-%-%-%-*.*$") then
      if last then
        table.remove(content)
      end
			insert(block)
			local decorator, caption = string.match(line, "^(%-%-%-%-*)(.*)$")
			if last then
				table.insert(blocks, {
					block = HEADING,
					title = last,
					caption = caption,
					decorator = decorator,
					file = options.file,
					line = index - 1,
          active = true
				})
			else
				table.insert(errors, {
					message = locale(options, "Title not specified"),
					file = options.file,
					line = index
				})
			end
			last = nil
		elseif not code.block and string.find(line, "^====*.*$") then
      if last then
        table.remove(content)
      end
			insert(block)
			local decorator, caption = string.match(line, "^(====*)(.*)$")
			if last then
				table.insert(blocks, {
					block = HEADING,
					title = last,
					caption = caption,
					decorator = decorator,
					file = options.file,
					line = index - 1,
          active = true
				})
			else
				table.insert(errors, {
					message = locale(options, "Title not specified"),
					file = options.file,
					line = index
				})
			end
			last = nil
		elseif not code.block and string.find(line, "^%[+.-%]+.*$") then
			insert(block)
			local decorator, main, caption = string.match(line, "^(%[+)(.-)%]+(.*)$")
			table.insert(blocks, {
				block = PASSAGE,
				title = main,
				caption = caption,
				decorator = decorator,
				file = options.file,
				line = index,
        active = true
			})
			last = nil
		elseif not code.block and string.find(line, "^%s%s%s%s.*$") then
			local decorator, main = string.match(line, "^(%s%s%s%s)(.*)$")
			if block == TEXT then
				insert(block)
				block = CODE
				code = {
					block = false,
					caption = "",
          decorator = decorator
				}
			end
			table.insert(content, main)
			last = nil
		elseif string.find(line, "^````*.*$") then
			local decorator, caption = string.match(line, "^(````*)(.*)$")
			if block == TEXT then
				insert(block)
				block = CODE
				code = {
					block = true,
					caption = caption,
          decorator = decorator
				}
			elseif block == CODE then
				insert(block)
				block = TEXT
			end
			last = nil
    elseif string.find(line, "^~~~~*.*$") then
			local decorator, caption = string.match(line, "^(~~~~*)(.*)$")
			if block == TEXT then
				insert(block)
				block = CODE
				code = {
					block = true,
					caption = caption,
          decorator = decorator
				}
			elseif block == CODE then
				insert(block)
				block = TEXT
			end
			last = nil
		elseif block == TEXT and last == "" and string.find(line, "^$") then
			insert(block)
			last = nil
		else
			if block == CODE and not code.block then
				insert(block)
				block = TEXT
			end
			if block == TEXT then
				table.insert(content, line)
				last = line
			elseif code.block then
				table.insert(content, line)
				last = nil
			end
		end

	end

	index = index + 1
	insert(block)

	return blocks, errors

end

local multi = {}
local multi_mt = {
	__index = multi
}

multi.localization = localization
multi.configuration = configuration

multi.bind = bind

function multi.new(blocks, options)
	local self = {}

  self.options = table_merge({
    tags = configuration.tags,
		events = {
			["error"] = configuration.events["error"],
      ["notice"] = configuration.events["notice"],
			["load"] = configuration.events["load"],
			["save"] = configuration.events["save"],
			["section-open"] = configuration.events["section-open"],
			["section-close"] = configuration.events["section-close"],
      ["play"] = configuration.events["play"],
			["close"] = configuration.events["close"],
			["query"] = configuration.events["query"],
      ["text"] = configuration.events["text"]
		},
		localization = configuration.localization,
		state = configuration.state,
		free = configuration.free,
    handlers = configuration.handlers
	}, options or {})

  local errors

  blocks = blocks or {}

  if type(blocks) == "string" then
    blocks, errors = multi.bind(blocks, options)
    for _, value in pairs(errors) do
      self.options.events["error"](self, value.message, value)
    end
  end

  self.data = {}

	self.blocks = {}
	self.stacks = {}
	self.states = {}
  self.sandboxes = {}

	for index = 1, #blocks do
		self.blocks[#self.blocks + 1] = blocks[index]
	end

	return setmetatable(self, multi_mt)
end

function multi:play(user)
	user = user or "user"

	self:load(user)

	self:run(user)

end

function multi:load(user)
  local data = self.options.events["load"](self, user)

  if data then
    self.states[user] = table_merge({}, data)
  else
    self.states[user] = table_merge({}, self.options.state)
  end

  self.stacks[user] = {}
  self.sandboxes[user] = table_merge({}, whitelist)

  self:define(user, localization[self.options.localization].code["user"], self.states[user].data)

  self:define(user, localization[self.options.localization].code["reply"], false)

  self:define(user, localization[self.options.localization].code["replies"], function(_, section, list)
    local historic = self.states[user].historic
    local replies = {}
    for _, value in pairs(historic) do
      if value.heading == section then
        if value.passage then
          table.insert(replies, 1, value.passage)
        end
      end
    end
    if list then
      return replies
    end
    return replies[1] or false
  end)

  self:define(user, localization[self.options.localization].code["write"], function(_, ...)
    local blocks, errors = multi.bind(table.concat({...}, "\n"), table_merge({
      included = true
    }, self.options))
    for _, value in pairs(errors) do
      self.options.events["notice"](self, user, value.message, value)
    end
    for _, value in pairs(blocks) do
      self.stacks[user][#self.stacks[user] + 1] = value
    end
    return true
  end)

  self:define(user, localization[self.options.localization].code["move"], function(_, section)
    local heading_index = self:search(user, {
      block = HEADING,
      title = section
    }, 1)

    if heading_index then
      self.stacks[user][#self.stacks[user] + 1] = {
        block = MOVE,
        index = heading_index
      }
      return true
    else
      self.options.events["notice"](self, user, locale(self.options, "Section not found (%s)", section))
    end
    return false
  end)

  self:define(user, localization[self.options.localization].code["visit"], function(_, section)
    local heading_index = self:search(user, {
      block = HEADING,
      title = section
    }, 1)

    if heading_index then
      local heading_next_index = self:search(user, {
        block = HEADING
      }, heading_index)

      self:search(user, {}, heading_index + 1, function(index, block)
        if not heading_next_index or index < heading_next_index then
          self.stacks[user][#self.stacks[user] + 1] = table_merge({}, block)
        else
          return true
        end
      end)
      return true
    end

    self.options.events["notice"](self, user, locale(self.options, "Section not found (%s)", section))
    return false
  end)

  self:define(user, localization[self.options.localization].code["set"], function(_, key, value)
    self.states[user].data[key] = value
    return value
  end)

  self:define(user, localization[self.options.localization].code["get"], function(_, key)
    return self.states[user].data[key]
  end)

end

function multi:define(user, key, value)
  if not user then
    self.options.events["error"](self, locale(self.options, "User not specified"))
    return false
  end
  if not self.sandboxes[user] then
    self:load(user)
  end
  if type(value) == "function" then
    self.sandboxes[user][key] = function(...)
      value({
        user = user,
        state = self.states[user],
        stacks = self.stacks[user],
        sandbox = self.sandboxes[user]
      }, ...)
    end
  else
    self.sandboxes[user][key] = value
  end
  return true
end

function multi:run(user)

	if not user then
		self.options.events["error"](self, locale(self.options, "User not specified"))
		return
	end

	if not self.states[user] then
		self:play(user)
		return
	end

	if not self.stacks[user] then
		self.stacks[user] = {}
	end

	local state = self.states[user]

	local last = state.historic[#state.historic]

  local index, block

  local last_index, last_block

  if last then
    last_index, last_block = self:search(user, {
			block = HEADING,
			title = last.heading
		})

    if not last_index then
      self.options.events["notice"](self, user, locale(self.options, "Section not found (%s)", last.heading))
    end
  end

	if not last then
    self.options.events["play"](self, user)

    index = 1
    block = self:select(user, index)

    if not block then
      self.options.events["close"](self, user)
      return
    end
	elseif last.status == STARTED or last.status == SELECTED then
    self.options.events["notice"](self, user, locale(self.options, "Section in execution (%s)", last.heading))
    return
	elseif last.status == QUERIED then
    return
	elseif last.status == REPLIED then
    index, block = last_index, last_block

    if not index then
      return
    end

    local passage_index, passage_block = self:search(user, {
      block = PASSAGE,
      title = last.passage
    }, index)

    if not passage_index then
      passage_index, passage_block = self:search(user, {
        block = PASSAGE,
        title = self.options.free
      }, index)
    end

    local heading_next_index = self:search(user, {
      block = HEADING
		}, index + 1)

    if (not passage_index) or (heading_next_index and passage_index > heading_next_index) then
      self.options.events["notice"](self, user, locale(self.options, "Passage '%s' not found in section '%s'", last.passage, last.heading))
      return
    end

    index, block = passage_index, passage_block

	elseif last.status == COMPLETE then

    if not last_index then
      return
    end

    index, block = self:search(user, {
			block = HEADING
		}, last_index + 1)

    if not index then
      self.options.events["close"](self, user)
      return
    end
	end

  if last and not last.status then
    last.status = STARTED
    index, block = self:search(user, {}, last_index + 1)

    if not index  then
      self.options.events["close"](self, user)
      return
    end
  end

  local query

	while block do

    local next

		if block.block == HEADING then

			if last then
        last.status = COMPLETE
				self.options.events["section-close"](self, user, table_merge({}, last_block))
			end
			table.insert(state.historic, {
				heading = block.title
			})
			self.options.events["save"](self, user, state)
			self.options.events["section-open"](self, user, table_merge({}, block))

			self:run(user)
			return

    elseif block.block == PASSAGE then

			if last and last.status == REPLIED then
        last.status = SELECTED
        -- deixe rolar!!
      elseif last and last.status == SELECTED then
        -- agora deve selecionar o proximo heading
        local heading_next_index = self:search(user, {
					block = HEADING
				}, index)

        if heading_next_index then
          next = heading_next_index
        else
          self.options.events["close"](self, user)
          return
        end

      elseif last and last.status == STARTED then

        local list = {}
        local passages = {}
        local free = false

        local heading_next_index = self:search(user, {
					block = HEADING
				}, index)

        self:search(user, {
					block = PASSAGE
				}, index, function(search_index, search_block)
          if not heading_next_index or search_index < heading_next_index then
            if self.options.free == search_block.title then
              free = true
            else
              table.insert(list, search_block.title)
              table.insert(passages, table_merge({}, search_block))
            end
          else
            return true
          end
        end)

        last.status = QUERIED

        self.options.events["save"](self, user, state)

        self.options.events["query"](self, user, table_merge({
          passages = passages,
          list = list,
          query = query,
          free = free
        }, {
          section = last_block
        }))

        return

      end

      query = nil

    elseif block.block == TEXT then

      local valid = true

      if last and last.status == STARTED then

        self:search(user, {
          block = PASSAGE
        }, index, function(next_index)
          if next_index == index + 1 then
            local data = table_merge({}, block)

            query = table_merge(data, {
              content = self:replace(user, block.content)
            })
            valid = false
          end
          return true
        end)

      end

      if valid then

        query = nil

        local data = table_merge({}, block)

        if last then
          data = table_merge(data, {
            section = last_block
         })
        end

        self.options.events["text"](self, user, table_merge(data, {
          content = self:replace(user, block.content)
        }))

      end

    elseif block.block == CODE then

      query = nil

      local status, result = self.options.handlers.code_loader(self, user, block)

      if status then
        status, result = pcall(result)
        if not status then
          self.options.events["notice"](self, user, locale(self.options, "Code error (%s)", result), block)
        end
      else
        self.options.events["notice"](self, user, locale(self.options, "Code error (%s)", result), block)
      end

    elseif block.block == MOVE then
      next = block.index
    end

    if next then
      index = next
    else
      index = index + 1
    end

		block = self:select(user, index)

  end

  self.options.events["close"](self, user)

end

function multi:select(user, index)
  if #self.stacks[user] > 0 then
    return table.remove(self.stacks[user], 1)
  end
	if index > 0 and self.blocks[index] then
		return self.blocks[index]
	end
	return false
end

function multi:search(user, compare, start, handler)
  if compare.active == nil then
    compare.active = true
  end
	if #self.stacks[user] > 0 then
		for index = 1, #self.stacks[user] do
			local valid = true
      for key, value in pairs(compare) do
        if self.stacks[user][index][key] ~= value then
          valid = false
          break
        end
      end
			if valid then
				 if handler then
					 if handler(- (#self.stacks[user] - index), self.stacks[user][index]) then
						 return - (#self.stacks[user] - index), self.stacks[user][index]
					 end
				 else
					 return - (#self.stacks[user] - index), self.stacks[user][index]
				 end
			end
		end
	end
	for index = start or 1, #self.blocks do
		local valid = true
    if self.blocks[index] then
      for key, value in pairs(compare) do
        if self.blocks[index][key] ~= value then
          valid = false
          break
        end
      end
    else
      valid = false
    end
		if valid then
      if handler then
        if handler(index, self.blocks[index]) then
          return index, self.blocks[index]
        end
      else
        return index, self.blocks[index]
      end
		end
	end
	return false
end

function multi:replace(user, text)
  for _, tag in pairs(self.options.tags) do
    local open, close
    local replace = "return tostring(%s)"
    if type(tag) == "string" then
      open, close = tag, tag
    elseif type(tag) == "table" and #tag >= 2 then
      open, close = tag[1], tag[2]
      if tag[3] then
        replace = tag[3]
      end
    end
    if open and close then
      string.gsub(text, string.format("%s(.-)%s", open, close), function(code)
        local code_block = {
          content = string.format(replace, code),
          text = text
        }
        local status, result = self.options.handlers.code_loader(self, user, code_block)
        if status then
          return result()
        else
          self.options.events["notice"](self, user, locale(self.options, "Code error (%s)", result), code_block)
        end
      end)
    end
  end
end

function multi:on(event, callback)
  if not event then
    self.options.events["error"](self, locale(self.options, "Event not specified"))
    return
  end
  if callback and type(callback) ~= "function" then
    self.options.events["error"](self, locale(self.options, "Invalid callback"))
    return
  end
	self.options.events[event] = callback
end

function multi:reply(user, reply)
  if not user then
    self.options.events["error"](self, locale(self.options, "User not specified"))
    return
  end
  if not reply or type(reply) ~= "string" then
    self.options.events["notice"](self, user, locale(self.options, "Invalid reply"))
    return
  end
  if not self.states[user] then
    self:load(user)
  end

  local state = self.states[user]
  local last = state.historic[#state.historic]

  if not last then
    self.options.events["notice"](self, user, locale(self.options, "Unexpected reply"))
    return
  end

  last.status = REPLIED
  last.passage = reply

  self:define(user, localization[self.options.localization].code["reply"], reply)

  self.options.events["save"](self, user, state)

  self:run(user)
end

multi._VERSION = "v" .. VERSION
multi._URL = "https://github.com/olueiro/multi"
multi._LICENSE = "MIT <https://opensource.org/licenses/MIT>"
multi._DESCRIPTION = "Markdown to nonlinear narrative"



--[==[
local b = bind([=[
testinggg


~~~
write("[aqui]")
~~~


teste
-----

~~~
write("[aqui]")
~~~
]=])

local test = multi.new(b)
test:play('me')
--]==]
--[==[
local cli = false

if getfenv then
  print("aqui2")
  -- Lua 5.1
  if pcall(getfenv, 3) then
    cli = true
  end
elseif debug and debug.getinfo then
  print("aqui")
  -- Lua 5.2, lua.vm.js, LuaJIT
  if debug.getlocal(4, 1) then
    cli = true
  end
else
  local status, debug = pcall(require, "debug")
  if status and debug and debug.getinfo then
    if debug.getinfo(4) then
      cli = true
    end
  end
end
print(cli)
--]==]

--[==[
if true then
  local function parse(data)
    local result = {}
    for key, value in pairs(data) do
      table.insert(result, string.format("%s: %s", tostring(key), tostring(value)))
    end
    return string.format("\n(%s)\n", table.concat(result, ", "))
  end

  local main = multi.new([[
teste
-----

Você é maior de idade?

[sim]

[não]

teste 2
-------

Deseja mesmo acessar esse site?

[sim]

[não]
]])
  main:on("error", function(message, details)
    io.write(message, parse(details))
  end)
  main:on("notice", function(user, message, details)
    io.write(message, parse(details))
  end)
  main:on("query", function(parent, user, data)
    io.write(data.query.content, "\n")
    for index, value in pairs(data.list) do
      io.write(index .. ")", value, "\n\n")
    end
    local reply
    local function post()
      if data.free then
        io.write(locale(main.options, "Write or choose a reply"), "\n\n")
      else
        io.write(locale(main.options, "Choose a reply"), "\n\n")
      end
      reply = io.read()
      if data.free and reply ~= "" then
        main:reply(user, reply)
      elseif tonumber(reply) and data.list[tonumber(reply)] then
        main:reply(user, data.list[tonumber(reply)])
      else
        io.write("\n\n", locale(main.options, "Invalid reply"), "\n")
        post()
      end
    end
    if not reply then
      post()
    end
  end)
  main:on("text", function(parent, user, data)
    io.write(data.content, "\n")
  end)
  main:on("section-open", function(parent, user, data)
    io.write("\n\n", data.title, "\n\n")
  end)
  main:on("close", function(parent, user)
    io.write("\n\n", locale(main.options, "Complete"), "\n")
  end)
  main:play("")
end
--]==]
return multi
