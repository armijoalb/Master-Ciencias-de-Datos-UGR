function  [train,labels] = createTestFold(h,m,ind)
  [_,folds] = size(h);
  train = [];
  labels = [];
  for i = 1:folds
    if i != ind
      [rows_h,cols_h] = size(h(i).histogram);
      [rows_m,cols_m] = size(m(i).histogram);
      train = vertcat(train,h(i).histogram);
      train = vertcat(train,m(i).histogram);
      labels = horzcat(labels,ones(1,rows_h)*-1);
      labels = horzcat(labels,ones(1,rows_m));
    end
  end
  
  labels = labels';
      
end
