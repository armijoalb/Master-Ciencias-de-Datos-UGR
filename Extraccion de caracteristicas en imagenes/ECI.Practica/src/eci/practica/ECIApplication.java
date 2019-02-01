package eci.practica;

import javax.swing.JFrame;
import javax.swing.UIManager;
import org.opencv.core.Core;

/**
 *
 * @author Jesús Chamorro Martínez (jesus@decsai.ugr.es)
 */
public class ECIApplication {

    static{ System.loadLibrary(Core.NATIVE_LIBRARY_NAME); }
    
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        //<editor-fold defaultstate="collapsed" desc=" Look and feel setting code (optional) ">       
        //setNimbusLF();
        //</editor-fold>        
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            JFrame.setDefaultLookAndFeelDecorated(true);
        } catch (Exception e) {
        }      
        ECIFrame ventana =  new ECIFrame();
        ventana.setSize(1300,800);
        ventana.setLocationRelativeTo(null);
        ventana.splitPanelCentral.setDividerLocation(0.8);
        ventana.setVisible(true);   
    }
    
}
