object dmMenu: TdmMenu
  OldCreateOrder = False
  Height = 214
  Width = 349
  object menuLeft: TksSlideMenu
    Appearence.BackgroundColor = xFF545454
    Appearence.HeaderColor = xFF424242
    Appearence.HeaderFontColor = xFFDADADA
    Appearence.ItemColor = xFF545454
    Appearence.Theme = mtCustom
    OnBuildMenu = menuLeftBuildMenu
    Left = 48
    Top = 40
  end
  object menuRight: TksSlideMenu
    Appearence.BackgroundColor = xFF2A7A9D
    Appearence.HeaderColor = xFF2A7A9D
    Appearence.HeaderFontColor = xFFC7FFFB
    Appearence.ItemColor = xFF2A7A9D
    Appearence.SelectedItemColor = xFF1A5670
    Appearence.Theme = mtDarkBlue
    OnBuildMenu = menuRightBuildMenu
    Left = 136
    Top = 40
  end
end
