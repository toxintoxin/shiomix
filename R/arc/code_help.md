必须以aggr$data_l_f <- aggr$data_l %>% 作为开头  (解释：上方Label得到的数据框为aggr$data_l, 下方Summarise以aggr$data_l_f开始)  
实际上可以是任何操作，如果上方Label或者下方Summarise自定义程度不够满足要求，全都在这里可以用代码实现  
现在提供一些基本的操作dplyr::filter()  