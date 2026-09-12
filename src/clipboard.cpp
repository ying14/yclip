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

  UINT format = 0;

  while ((format = EnumClipboardFormats(format)) != 0) {

    ids.push_back(format);

    std::string name;
    std::string type;
    bool is_standard = false;

    // Standard Windows clipboard formats
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

    default:
      // Registered format
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

    names.push_back(name);
    types.push_back(type);
    standard.push_back(is_standard);
  }

  DWORD error = GetLastError();

  CloseClipboard();

  // ERROR_SUCCESS means enumeration simply reached the end.
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
    Rcpp::_["standard"] = standard
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
  // Registered format
  return RegisterClipboardFormatA(format_name.c_str());
}


// [[Rcpp::export]]
Rcpp::RawVector clipboard_read_raw_windows(std::string format_name) {

  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }

  // Find the clipboard format ID from its name
  UINT format = get_clipboard_format_id(format_name);
  // UINT format = RegisterClipboardFormatA(format_name.c_str());

  if (format == 0) {
    CloseClipboard();
    Rcpp::stop("Could not find clipboard format: %s", format_name);
  }

  HANDLE hData = GetClipboardData(format);

  if (hData == NULL) {
    CloseClipboard();
    Rcpp::stop(
      "Clipboard format not available: %s", format_name
    );
  }

  SIZE_T size = GlobalSize(hData);

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



// [[Rcpp::export]]
void clipboard_write_raw_windows(
    Rcpp::RawVector data,
    std::string format_name
) {

  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }

  UINT format = get_clipboard_format_id(format_name);

  if (format == 0) {
    CloseClipboard();
    Rcpp::stop("Could not find clipboard format: %s", format_name);
  }

  HGLOBAL hData = GlobalAlloc(
    GMEM_MOVEABLE,
    data.size()
  );

  if (hData == NULL) {
    CloseClipboard();
    Rcpp::stop("Could not allocate clipboard memory.");
  }

  void* ptr = GlobalLock(hData);

  if (ptr == NULL) {
    GlobalFree(hData);
    CloseClipboard();
    Rcpp::stop("Could not lock clipboard memory.");
  }

  std::memcpy(
    ptr,
    data.begin(),
    data.size()
  );

  GlobalUnlock(hData);

  if (!EmptyClipboard()) {
    GlobalFree(hData);
    CloseClipboard();
    Rcpp::stop("Could not empty clipboard.");
  }

  if (SetClipboardData(format, hData) == NULL) {
    GlobalFree(hData);
    CloseClipboard();
    Rcpp::stop("Could not set clipboard data.");
  }

  // Windows now owns hData.
  CloseClipboard();
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
      Rcpp::stop(
        "Data for clipboard format '%s' must be a raw vector.",
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

    Rcpp::stop(
      "Could not open clipboard."
    );
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

    Rcpp::stop(
      "Could not empty clipboard."
    );
  }


  // ------------------------------------------------------------
  // Set each clipboard format.
  //
  // IMPORTANT:
  // Successful SetClipboardData() transfers ownership of
  // the HGLOBAL to Windows.
  // ------------------------------------------------------------

  for (R_xlen_t i = 0; i < data.size(); ++i) {

    if (SetClipboardData(
        formats[i],
               handles[i]
    ) == NULL) {

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
