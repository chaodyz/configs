local pattern = [[%s*(%d+):(%d+)%s+(%w+)%s+(.+%S)%s+(%S+)]]
local groups = { 'lnum', 'col', 'severity', 'message', 'code' }
local severity_map = {
  ['error'] = vim.diagnostic.severity.ERROR,
  ['warn'] = vim.diagnostic.severity.WARN,
  ['warning'] = vim.diagnostic.severity.WARN,
}

return {
  cmd = function()
    local local_tslint = vim.fn.fnamemodify('./node_modules/.bin/tslint', ':p')
    local stat = vim.loop.fs_stat(local_tslint)
    if stat then
      return local_tslint
    end
    return 'tslint'
  end,
  args = {},
  stdin = false,
  stream = 'stdout',
  ignore_exitcode = true,
  parser = require('lint.parser').from_pattern(pattern, groups, severity_map, { ['source'] = 'tslint' }),
}
