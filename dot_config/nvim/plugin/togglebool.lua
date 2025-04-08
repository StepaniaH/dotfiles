local function toggle_bool()
  local word = vim.fn.expand('<cword>')
  local toggle_word

  -- Determine the toggled word
  if word == 'true' then
    toggle_word = 'false'
  elseif word == 'True' then
    toggle_word = 'False'
  elseif word == 'false' then
    toggle_word = 'true'
  elseif word == 'False' then
    toggle_word = 'True'
  else
    return
  end

  local line = vim.fn.getline('.')
  local escaped_word = vim.fn.escape(word, '\\')
  local pattern = '\\<' .. escaped_word .. '\\>'

  local match_info = vim.fn.matchstrpos(line, pattern)
  local match_start = match_info[2]
  local match_end = match_info[3]

  local lua_start = match_start + 1 -- Convert to 1-based index
  local lua_end = match_end        -- `match_end` is the index after the match in 0-based indexing

  local cursor_col = vim.fn.col('.') - 1 -- 0-based

  if cursor_col >= match_start and cursor_col < match_end then
    local prefix = line:sub(1, lua_start - 1)
    local suffix = line:sub(lua_end + 1) -- Corrected here
    local new_line = prefix .. toggle_word .. suffix
    vim.fn.setline('.', new_line)
    return
  end

  -- Modified loop with an additional check to prevent infinite looping
  local prev_match_end = match_end
  while true do
    -- Search for the next match starting from the previous match's end index
    match_info = vim.fn.matchstrpos(line, pattern, prev_match_end)
    match_start = match_info[2]
    match_end = match_info[3]

    -- If no match is found, or the match didn't advance, break the loop
    if match_info[1] == -1 or match_start <= prev_match_end then
      break
    end

    -- Update previous match end index
    prev_match_end = match_end

    -- Check if the cursor is within the new match
    if cursor_col >= match_start and cursor_col < match_end then
      local lua_start = match_start + 1
      local lua_end = match_end
      local prefix = line:sub(1, lua_start - 1)
      local suffix = line:sub(lua_end + 1) -- Corrected here
      local new_line = prefix .. toggle_word .. suffix
      vim.fn.setline('.', new_line)
      return
    end
  end
end

vim.api.nvim_create_user_command('ToggleBool', toggle_bool, {})
