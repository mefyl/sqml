type nonrec ('t, 'tag) t = ('t, 'tag) Expression.column

module List = S_list.Make2 (struct
  type nonrec ('a, 'tag) t = ('a, 'tag) t
end)
