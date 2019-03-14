function  [test,labels_test] = createTestFold(h,m,ind)
  test = h(ind).histogram;
  test = [test;m(ind).histogram];
  [h_r,h_c] = size(h(ind).histogram);
  [m_r,m_c] = size(m(ind).histogram);
  labels_test = ones(1,h_r)*-1;
  labels_test = [labels_test,ones(1,m_r)];
  labels_test = labels_test';
end
