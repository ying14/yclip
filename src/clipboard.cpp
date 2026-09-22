#include <Rcpp.h>
#include <windows.h>
#include <cstring>


//' @title Inspect Clipboard on Windows
//'
//' @return data frame showing the various formats available on the clipboard
//' @export
// [[Rcpp::export]]
Rcpp::DataFrame clipboard_inspect_windows() {
  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open the Windows clipboard.");
  }
  std::vector<unsigned int> ids;
  std::vector<std::string> names;
  std::vector<std::string> types;
  std::vector<bool> standard;
  std::vector<bool> available;
  UINT format = 0;
  while ((format = EnumClipboardFormats(format)) != 0) {
    ids.push_back(format);
    std::string name;
    std::string type;
    bool is_standard = false;
    switch (format) {
    case CF_TEXT:
      name = "CF_TEXT";
      type = "text";
      is_standard = true;
      break;
    case CF_BITMAP:
      name = "CF_BITMAP";
      type = "image";
      is_standard = true;
      break;
    case CF_DIB:
      name = "CF_DIB";
      type = "image";
      is_standard = true;
      break;
    case CF_DIBV5:
      name = "CF_DIBV5";
      type = "image";
      is_standard = true;
      break;
    case CF_UNICODETEXT:
      name = "CF_UNICODETEXT";
      type = "text";
      is_standard = true;
      break;
    case CF_HDROP:
      name = "CF_HDROP";
      type = "file";
      is_standard = true;
      break;
    case CF_OWNERDISPLAY:
      name = "CF_OWNERDISPLAY";
      type = "special";
      is_standard = true;
      break;
    case CF_ENHMETAFILE:
      name = "CF_ENHMETAFILE";
      type = "image";
      is_standard = true;
      break;
    case CF_LOCALE:
      name = "CF_LOCALE";
      type = "locale";
      is_standard = true;
      break;
    case CF_METAFILEPICT:
      name = "CF_METAFILEPICT";
      type = "image";
      is_standard = true;
      break;
    case CF_PENDATA:
      name = "CF_PENDATA";
      type = "binary";
      is_standard = true;
      break;
    case CF_OEMTEXT:
      name = "CF_OEMTEXT";
      type = "text";
      is_standard = true;
      break;
    default:
      char buffer[256];
    int length = GetClipboardFormatNameA(
      format,
      buffer,
      sizeof(buffer)
    );
    if (length > 0) {
      name = std::string(buffer, length);
    } else {
      name = "Unknown";
    }
    type = "registered";
    is_standard = false;
    }
    HANDLE hData = GetClipboardData(format);
    bool is_available = (hData != NULL);
    names.push_back(name);
    types.push_back(type);
    standard.push_back(is_standard);
    available.push_back(is_available);
  }
  DWORD error = GetLastError();
  CloseClipboard();
  if (error != ERROR_SUCCESS) {
    Rcpp::stop(
      "Error while enumerating clipboard formats. Windows error: %lu",
      error
    );
  }
  return Rcpp::DataFrame::create(
    Rcpp::_["id"] = ids,
    Rcpp::_["name"] = names,
    Rcpp::_["type"] = types,
    Rcpp::_["standard"] = standard,
    Rcpp::_["available"] = available
  );
}



UINT get_clipboard_format_id(const std::string& format_name) {
  if (format_name == "CF_TEXT")
    return CF_TEXT;
  if (format_name == "CF_BITMAP")
    return CF_BITMAP;
  if (format_name == "CF_METAFILEPICT")
    return CF_METAFILEPICT;
  if (format_name == "CF_SYLK")
    return CF_SYLK;
  if (format_name == "CF_DIF")
    return CF_DIF;
  if (format_name == "CF_TIFF")
    return CF_TIFF;
  if (format_name == "CF_OEMTEXT")
    return CF_OEMTEXT;
  if (format_name == "CF_DIB")
    return CF_DIB;
  if (format_name == "CF_PALETTE")
    return CF_PALETTE;
  if (format_name == "CF_PENDATA")
    return CF_PENDATA;
  if (format_name == "CF_RIFF")
    return CF_RIFF;
  if (format_name == "CF_WAVE")
    return CF_WAVE;
  if (format_name == "CF_UNICODETEXT")
    return CF_UNICODETEXT;
  if (format_name == "CF_ENHMETAFILE")
    return CF_ENHMETAFILE;
  if (format_name == "CF_HDROP")
    return CF_HDROP;
  if (format_name == "CF_LOCALE")
    return CF_LOCALE;
  if (format_name == "CF_DIBV5")
    return CF_DIBV5;
  return RegisterClipboardFormatA(format_name.c_str());
}


bool clipboard_format_is_hglobal(UINT format) {
  switch (format) {
  case CF_TEXT:
  case CF_METAFILEPICT:
  case CF_SYLK:
  case CF_DIF:
  case CF_TIFF:
  case CF_OEMTEXT:
  case CF_DIB:
  case CF_PENDATA:
  case CF_RIFF:
  case CF_WAVE:
  case CF_UNICODETEXT:
  case CF_HDROP:
  case CF_LOCALE:
  case CF_DIBV5:
    return true;
  case CF_BITMAP:
  case CF_PALETTE:
  case CF_ENHMETAFILE:
  case CF_OWNERDISPLAY:
    return false;
  default:
    // Registered formats are not necessarily HGLOBAL, but most
    // application-defined clipboard data is stored this way.
    return true;
  }
}


// [[Rcpp::export]]
void clipboard_isolate_windows(Rcpp::List formats) {
  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }
  struct ClipboardData {
    UINT format;
    std::string name;
    std::vector<unsigned char> data;
  };
  std::vector<ClipboardData> saved;
  for (R_xlen_t i = 0; i < formats.size(); ++i) {
    SEXP input = formats[i];
    UINT format;
    std::string name;
    if (TYPEOF(input) == STRSXP) {
      Rcpp::CharacterVector x(input);
      if (x.size() != 1 || Rcpp::CharacterVector::is_na(x[0])) {
        CloseClipboard();
        Rcpp::stop(
          "Each clipboard format must be a single non-missing character string or integer ID."
        );
      }
      name = Rcpp::as<std::string>(x[0]);
      format = get_clipboard_format_id(name);
    } else if (TYPEOF(input) == INTSXP) {
      Rcpp::IntegerVector x(input);
      if (x.size() != 1 || Rcpp::IntegerVector::is_na(x[0]) ||
          x[0] < 1) {
        CloseClipboard();
        Rcpp::stop(
          "Each clipboard format must be a single non-missing character string or integer ID."
        );
      }
      format = static_cast<UINT>(x[0]);
      name = std::to_string(format);
    } else {
      CloseClipboard();
      Rcpp::stop(
        "Each clipboard format must be a character string or integer ID."
      );
    }

    if (format == 0) {
      CloseClipboard();
      Rcpp::stop("Could not find clipboard format: %s", name);
    }
    if (!clipboard_format_is_hglobal(format)) {
      CloseClipboard();
      Rcpp::stop(
        "Clipboard format %s (format ID %u) cannot be isolated "
        "because its clipboard handle is not an HGLOBAL.",
        name,
        format
      );
    }
    HANDLE hData = GetClipboardData(format);
    if (hData == NULL) {
      DWORD error = GetLastError();
      CloseClipboard();
      Rcpp::stop(
        "Clipboard format not available: %s (format ID %u, Windows error %lu)",
        name,
        format,
        error
      );
    }
    SIZE_T size = GlobalSize(hData);
    if (size == 0) {
      CloseClipboard();
      Rcpp::stop("Clipboard format %s has zero length.", name);
    }
    void* ptr = GlobalLock(hData);
    if (ptr == NULL) {
      CloseClipboard();
      Rcpp::stop("Could not lock clipboard format: %s", name);
    }
    ClipboardData item;
    item.format = format;
    item.name = name;
    item.data.resize(size);
    std::memcpy(item.data.data(), ptr, size);
    GlobalUnlock(hData);
    saved.push_back(std::move(item));
  }
  std::vector<HGLOBAL> handles;
  handles.reserve(saved.size());
  for (const auto& item : saved) {
    HGLOBAL hMem = GlobalAlloc(GMEM_MOVEABLE, item.data.size());
    if (hMem == NULL) {
      for (HGLOBAL h : handles) {
        GlobalFree(h);
      }
      CloseClipboard();
      Rcpp::stop(
        "Could not allocate memory for clipboard format: %s",
        item.name
      );
    }
    void* ptr = GlobalLock(hMem);
    if (ptr == NULL) {
      GlobalFree(hMem);
      for (HGLOBAL h : handles) {
        GlobalFree(h);
      }
      CloseClipboard();
      Rcpp::stop(
        "Could not lock allocated memory for clipboard format: %s",
        item.name
      );
    }
    std::memcpy(ptr, item.data.data(), item.data.size());
    GlobalUnlock(hMem);
    handles.push_back(hMem);
  }
  if (!EmptyClipboard()) {
    for (HGLOBAL h : handles) {
      GlobalFree(h);
    }
    CloseClipboard();
    Rcpp::stop("Could not empty clipboard.");
  }
  for (size_t i = 0; i < saved.size(); ++i) {
    if (SetClipboardData(saved[i].format, handles[i]) == NULL) {
      handles[i] = NULL;
      CloseClipboard();
      Rcpp::stop(
        "Could not restore clipboard format: %s (format ID %u).",
        saved[i].name,
        saved[i].format
      );
    }
    handles[i] = NULL;
  }
  CloseClipboard();
}


// [[Rcpp::export]]
Rcpp::RawVector clipboard_read_raw_windows(SEXP format_input) {
  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }
  UINT format;
  std::string format_name;
  if (TYPEOF(format_input) == STRSXP) {
    format_name = Rcpp::as<std::string>(format_input);
    format = get_clipboard_format_id(format_name);
  } else if (TYPEOF(format_input) == INTSXP) {
    format = Rcpp::as<UINT>(format_input);
    format_name = std::to_string(format);
  } else {
    CloseClipboard();
    Rcpp::stop("Clipboard format must be a character string or integer ID.");
  }
  if (format == 0) {
    CloseClipboard();
    Rcpp::stop("Could not find clipboard format: %s", format_name);
  }
  if (!clipboard_format_is_hglobal(format)) {
    CloseClipboard();
    Rcpp::stop(
      "Clipboard format %s (ID %u) cannot be read as raw data because "
      "its clipboard handle is not an HGLOBAL.",
      format_name,
      format
    );
  }
  HANDLE hData = GetClipboardData(format);
  if (hData == NULL) {
    DWORD error = GetLastError();
    CloseClipboard();
    Rcpp::stop(
      "Clipboard format not available: %s (format ID %u, Windows error %lu)",
      format_name,
      format,
      error
    );
  }

  Rcpp::Rcout << "Format ID: " << format << "\n";
  Rcpp::Rcout << "Handle: " << hData << "\n";
  SIZE_T size = GlobalSize(hData);
  Rcpp::Rcout << "GlobalSize: " << size << "\n";

  if (size == 0) {
    CloseClipboard();
    Rcpp::stop("Clipboard data has zero length.");
  }
  void* ptr = GlobalLock(hData);
  if (ptr == NULL) {
    CloseClipboard();
    Rcpp::stop("Could not lock clipboard data.");
  }
  Rcpp::RawVector result(size);
  std::memcpy(result.begin(), ptr, size);
  GlobalUnlock(hData);
  CloseClipboard();
  return result;
}



// [[Rcpp::export]]
void clipboard_write_formats_windows(Rcpp::List data) {
  // ------------------------------------------------------------
  // Validate input
  // ------------------------------------------------------------
  Rcpp::CharacterVector names = data.names();
  if (names.size() != data.size()) {
    Rcpp::stop(
      "Clipboard formats must be supplied as a named list."
    );
  }
  if (data.size() == 0) {
    Rcpp::stop(
      "At least one clipboard format must be supplied."
    );
  }
  // Resolve format IDs and validate data before modifying clipboard.
  std::vector<UINT> formats(data.size());
  for (R_xlen_t i = 0; i < data.size(); ++i) {
    std::string format_name =
      Rcpp::as<std::string>(names[i]);
    if (format_name.empty()) {
      Rcpp::stop(
        "Clipboard format names cannot be empty."
      );
    }
    if (!Rcpp::is<Rcpp::RawVector>(data[i])) {
      Rcpp::stop("Data for clipboard format '%s' must be a raw vector.",
        format_name
      );
    }
    Rcpp::RawVector bytes(data[i]);
    if (bytes.size() == 0) {
      Rcpp::stop(
        "Clipboard format '%s' contains zero bytes.",
        format_name
      );
    }
    UINT format = get_clipboard_format_id(format_name);
    if (format == 0) {
      Rcpp::stop(
        "Could not find clipboard format: %s",
        format_name
      );
    }
    formats[i] = format;
  }

  // ------------------------------------------------------------
  // Allocate all clipboard memory before opening/emptying
  // clipboard.
  // ------------------------------------------------------------
  std::vector<HGLOBAL> handles(
      data.size(),
      NULL
  );
  for (R_xlen_t i = 0; i < data.size(); ++i) {
    Rcpp::RawVector bytes(data[i]);
    HGLOBAL hData = GlobalAlloc(
      GMEM_MOVEABLE,
      bytes.size()
    );
    if (hData == NULL) {
      for (R_xlen_t j = 0; j < i; ++j) {
        if (handles[j] != NULL) {
          GlobalFree(handles[j]);
        }
      }
      Rcpp::stop(
        "Could not allocate clipboard memory."
      );
    }
    void* ptr = GlobalLock(hData);
    if (ptr == NULL) {
      GlobalFree(hData);
      for (R_xlen_t j = 0; j < i; ++j) {
        if (handles[j] != NULL) {
          GlobalFree(handles[j]);
        }
      }
      Rcpp::stop(
        "Could not lock clipboard memory."
      );
    }
    std::memcpy(
      ptr,
      bytes.begin(),
      bytes.size()
    );
    GlobalUnlock(hData);
    handles[i] = hData;
  }

  // ------------------------------------------------------------
  // Open clipboard
  // ------------------------------------------------------------
  if (!OpenClipboard(NULL)) {
    for (HGLOBAL hData : handles) {
      if (hData != NULL) {
        GlobalFree(hData);
      }
    }
    Rcpp::stop("Could not open clipboard.");
  }

  // ------------------------------------------------------------
  // Empty existing clipboard
  // ------------------------------------------------------------

  if (!EmptyClipboard()) {
    CloseClipboard();
    for (HGLOBAL hData : handles) {
      if (hData != NULL) {
        GlobalFree(hData);
      }
    }
    Rcpp::stop("Could not empty clipboard.");
  }

  // ------------------------------------------------------------
  // Set each clipboard format.
  //
  // IMPORTANT:
  // Successful SetClipboardData() transfers ownership of
  // the HGLOBAL to Windows.
  // ------------------------------------------------------------

  for (R_xlen_t i = 0; i < data.size(); ++i) {
    if (SetClipboardData(formats[i],handles[i]) == NULL) {
      std::string format_name =
        Rcpp::as<std::string>(names[i]);
      // This particular handle was not transferred.
      GlobalFree(handles[i]);
      CloseClipboard();
      Rcpp::stop(
        "Could not set clipboard format: %s",
        format_name
      );
    }
    // Windows now owns this handle.
    handles[i] = NULL;
  }

  // ------------------------------------------------------------
  // Done
  // ------------------------------------------------------------
  CloseClipboard();
}



// [[Rcpp::export]]
std::string clipboard_read_unicode_text_windows() {
  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }
  HANDLE hData = GetClipboardData(CF_UNICODETEXT);
  if (hData == NULL) {
    CloseClipboard();
    Rcpp::stop("CF_UNICODETEXT is not available.");
  }
  wchar_t* ptr = static_cast<wchar_t*>(GlobalLock(hData));
  if (ptr == NULL) {
    CloseClipboard();
    Rcpp::stop("Could not lock clipboard data.");
  }
  int required = WideCharToMultiByte(
    CP_UTF8,
    0,
    ptr,
    -1,
    NULL,
    0,
    NULL,
    NULL
  );
  if (required == 0) {
    GlobalUnlock(hData);
    CloseClipboard();
    Rcpp::stop("Could not convert clipboard text to UTF-8.");
  }
  std::string result(required, '\0');
  WideCharToMultiByte(
    CP_UTF8,
    0,
    ptr,
    -1,
    &result[0],
    required,
    NULL,
    NULL
  );
  GlobalUnlock(hData);
  CloseClipboard();
  // Remove the terminating NUL.
  result.pop_back();
  return result;
}


// [[Rcpp::export]]
Rcpp::RawVector clipboard_read_dib_windows() {
  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }
  HANDLE hData = GetClipboardData(CF_DIB);
  if (hData == NULL) {
    CloseClipboard();
    Rcpp::stop("CF_DIB is not available on the clipboard.");
  }
  SIZE_T size = GlobalSize(hData);
  if (size == 0) {
    CloseClipboard();
    Rcpp::stop("CF_DIB data has zero length.");
  }
  void* ptr = GlobalLock(hData);
  if (ptr == NULL) {
    CloseClipboard();
    Rcpp::stop("Could not lock CF_DIB data.");
  }
  Rcpp::RawVector result(size);
  std::memcpy(result.begin(), ptr, size);
  GlobalUnlock(hData);
  CloseClipboard();
  return result;
}


// [[Rcpp::export]]
Rcpp::List clipboard_inspect_dib_windows() {
  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }
  HANDLE hData = GetClipboardData(CF_DIB);
  if (hData == NULL) {
    CloseClipboard();
    Rcpp::stop("CF_DIB is not available on the clipboard.");
  }
  void* ptr = GlobalLock(hData);
  if (ptr == NULL) {
    CloseClipboard();
    Rcpp::stop("Could not lock CF_DIB data.");
  }
  BITMAPINFOHEADER* header =
    static_cast<BITMAPINFOHEADER*>(ptr);
  Rcpp::List result = Rcpp::List::create(
    Rcpp::_["header_size"] = header->biSize,
    Rcpp::_["width"] = header->biWidth,
    Rcpp::_["height"] = header->biHeight,
    Rcpp::_["planes"] = header->biPlanes,
    Rcpp::_["bits_per_pixel"] = header->biBitCount,
    Rcpp::_["compression"] = header->biCompression,
    Rcpp::_["image_size"] = header->biSizeImage
  );
  GlobalUnlock(hData);
  CloseClipboard();
  return result;
}



