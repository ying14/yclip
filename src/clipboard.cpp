#include <Rcpp.h>
#include <windows.h>
#include <cstring>

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



// [[Rcpp::export]]
Rcpp::RawVector clipboard_read_raw_windows(std::string format_name) {

  if (!OpenClipboard(NULL)) {
    Rcpp::stop("Could not open clipboard.");
  }

  // Find the clipboard format ID from its name
  UINT format = RegisterClipboardFormatA(format_name.c_str());

  if (format == 0) {
    CloseClipboard();
    Rcpp::stop("Could not register/find clipboard format: ", format_name);
  }

  HANDLE hData = GetClipboardData(format);

  if (hData == NULL) {
    CloseClipboard();
    Rcpp::stop(
      "Clipboard format not available: ", format_name
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
